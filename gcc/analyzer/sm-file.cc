/* A state machine for detecting misuses of <stdio.h>'s FILE * API.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "options.h"
#include "diagnostic-path.h"
#include "diagnostic-metadata.h"
#include "function.h"
#include "analyzer/analyzer.h"
#include "diagnostic-event-id.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/sm.h"
#include "analyzer/pending-diagnostic.h"
#include "analyzer/function-set.h"
#include "analyzer/analyzer-selftests.h"

#if ENABLE_ANALYZER

namespace ana {

namespace {

/* A state machine for detecting misuses of <stdio.h>'s FILE * API.  */

class fileptr_state_machine : public state_machine
{
public:
  fileptr_state_machine (logger *logger);

  bool inherited_state_p () const FINAL OVERRIDE { return false; }

  bool on_stmt (sm_context *sm_ctxt,
		const supernode *node,
		const gimple *stmt) const FINAL OVERRIDE;

  void on_condition (sm_context *sm_ctxt,
		     const supernode *node,
		     const gimple *stmt,
		     tree lhs,
		     enum tree_code op,
		     tree rhs) const FINAL OVERRIDE;

  bool can_purge_p (state_t s) const FINAL OVERRIDE;
  pending_diagnostic *on_leak (tree var) const FINAL OVERRIDE;

  /* Start state.  */
  state_t m_start;

  /* State for a FILE * returned from fopen that hasn't been checked for
     NULL.
     It could be an open stream, or could be NULL.  */
  state_t m_unchecked;

  /* State for a FILE * that's known to be NULL.  */
  state_t m_null;

  /* State for a FILE * that's known to be a non-NULL open stream.  */
  state_t m_nonnull;

  /* State for a FILE * that's had fclose called on it.  */
  state_t m_closed;

  /* Stop state, for a FILE * we don't want to track any more.  */
  state_t m_stop;
};

/* Base class for diagnostics relative to fileptr_state_machine.  */

class file_diagnostic : public pending_diagnostic
{
public:
  file_diagnostic (const fileptr_state_machine &sm, tree arg)
  : m_sm (sm), m_arg (arg)
  {}

  bool subclass_equal_p (const pending_diagnostic &base_other) const OVERRIDE
  {
    return same_tree_p (m_arg, ((const file_diagnostic &)base_other).m_arg);
  }

  label_text describe_state_change (const evdesc::state_change &change)
    OVERRIDE
  {
    if (change.m_old_state == m_sm.m_start
	&& change.m_new_state == m_sm.m_unchecked)
      // TODO: verify that it's the fopen stmt, not a copy
      return label_text::borrow ("opened here");
    if (change.m_old_state == m_sm.m_unchecked
	&& change.m_new_state == m_sm.m_nonnull)
      return change.formatted_print ("assuming %qE is non-NULL",
				     change.m_expr);
    if (change.m_new_state == m_sm.m_null)
      return change.formatted_print ("assuming %qE is NULL",
				     change.m_expr);
    return label_text ();
  }

protected:
  const fileptr_state_machine &m_sm;
  tree m_arg;
};

class double_fclose : public file_diagnostic
{
public:
  double_fclose (const fileptr_state_machine &sm, tree arg)
    : file_diagnostic (sm, arg)
  {}

  const char *get_kind () const FINAL OVERRIDE { return "double_fclose"; }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    return warning_at (rich_loc, OPT_Wanalyzer_double_fclose,
		       "double %<fclose%> of FILE %qE",
		       m_arg);
  }

  label_text describe_state_change (const evdesc::state_change &change)
    OVERRIDE
  {
    if (change.m_new_state == m_sm.m_closed)
      {
	m_first_fclose_event = change.m_event_id;
	return change.formatted_print ("first %qs here", "fclose");
      }
    return file_diagnostic::describe_state_change (change);
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    if (m_first_fclose_event.known_p ())
      return ev.formatted_print ("second %qs here; first %qs was at %@",
				 "fclose", "fclose",
				 &m_first_fclose_event);
    return ev.formatted_print ("second %qs here", "fclose");
  }

private:
  diagnostic_event_id_t m_first_fclose_event;
};

class file_leak : public file_diagnostic
{
public:
  file_leak (const fileptr_state_machine &sm, tree arg)
    : file_diagnostic (sm, arg)
  {}

  const char *get_kind () const FINAL OVERRIDE { return "file_leak"; }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    diagnostic_metadata m;
    /* CWE-775: "Missing Release of File Descriptor or Handle after
       Effective Lifetime". */
    m.add_cwe (775);
    return warning_meta (rich_loc, m, OPT_Wanalyzer_file_leak,
			 "leak of FILE %qE",
			 m_arg);
  }

  label_text describe_state_change (const evdesc::state_change &change)
    FINAL OVERRIDE
  {
    if (change.m_new_state == m_sm.m_unchecked)
      {
	m_fopen_event = change.m_event_id;
	return label_text::borrow ("opened here");
      }
    return file_diagnostic::describe_state_change (change);
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    if (m_fopen_event.known_p ())
      return ev.formatted_print ("%qE leaks here; was opened at %@",
				 ev.m_expr, &m_fopen_event);
    else
      return ev.formatted_print ("%qE leaks here", ev.m_expr);
  }

private:
  diagnostic_event_id_t m_fopen_event;
};

/* fileptr_state_machine's ctor.  */

fileptr_state_machine::fileptr_state_machine (logger *logger)
: state_machine ("file", logger)
{
  m_start = add_state ("start");
  m_unchecked = add_state ("unchecked");
  m_null = add_state ("null");
  m_nonnull = add_state ("nonnull");
  m_closed = add_state ("closed");
  m_stop = add_state ("stop");
}

/* Get a set of functions that are known to take a FILE * that must be open,
   and are known to not close it.  */

static function_set
get_file_using_fns ()
{
  // TODO: populate this list more fully
  static const char * const funcnames[] = {
    /* This array must be kept sorted.  */
    "__fbufsize",
    "__flbf",
    "__fpending",
    "__fpurge"
    "__freadable",
    "__freading",
    "__fsetlocking",
    "__fwritable",
    "__fwriting",
    "clearerr",
    "clearerr_unlocked",
    "feof",
    "feof_unlocked",
    "ferror",
    "ferror_unlocked",
    "fflush", // safe to call with NULL
    "fflush_unlocked",  // safe to call with NULL
    "fgetc",
    "fgetc_unlocked",
    "fgetpos",
    "fgets",
    "fgets_unlocked",
    "fgetwc_unlocked",
    "fgetws_unlocked",
    "fileno",
    "fileno_unlocked",
    "fprintf",
    "fputc",
    "fputc_unlocked",
    "fputs",
    "fputs_unlocked",
    "fputwc_unlocked",
    "fputws_unlocked",
    "fread_unlocked",
    "fseek",
    "fsetpos",
    "ftell",
    "fwrite_unlocked",
    "getc",
    "getc_unlocked",
    "getwc_unlocked",
    "putc",
    "putc_unlocked",
    "rewind",
    "setbuf",
    "setbuffer",
    "setlinebuf",
    "setvbuf",
    "ungetc",
    "vfprintf"
  };
  const size_t count
    = sizeof(funcnames) / sizeof (funcnames[0]);
  function_set fs (funcnames, count);
  return fs;
}

/* Return true if FNDECL is known to require an open FILE *, and is known
   to not close it.  */

static bool
is_file_using_fn_p (tree fndecl)
{
  function_set fs = get_file_using_fns ();
  return fs.contains_decl_p (fndecl);
}

/* Implementation of state_machine::on_stmt vfunc for fileptr_state_machine.  */

bool
fileptr_state_machine::on_stmt (sm_context *sm_ctxt,
				const supernode *node,
				const gimple *stmt) const
{
  if (const gcall *call = dyn_cast <const gcall *> (stmt))
    if (tree callee_fndecl = sm_ctxt->get_fndecl_for_call (call))
      {
	if (is_named_call_p (callee_fndecl, "fopen", call, 2))
	  {
	    tree lhs = gimple_call_lhs (call);
	    if (lhs)
	      {
		lhs = sm_ctxt->get_readable_tree (lhs);
		sm_ctxt->on_transition (node, stmt, lhs, m_start, m_unchecked);
	      }
	    else
	      {
		/* TODO: report leak.  */
	      }
	    return true;
	  }

	if (is_named_call_p (callee_fndecl, "fclose", call, 1))
	  {
	    tree arg = gimple_call_arg (call, 0);
	    arg = sm_ctxt->get_readable_tree (arg);

	    sm_ctxt->on_transition (node, stmt, arg, m_start, m_closed);

	    // TODO: is it safe to call fclose (NULL) ?
	    sm_ctxt->on_transition (node, stmt, arg, m_unchecked, m_closed);
	    sm_ctxt->on_transition (node, stmt, arg, m_null, m_closed);

	    sm_ctxt->on_transition (node, stmt , arg, m_nonnull, m_closed);

	    sm_ctxt->warn_for_state (node, stmt, arg, m_closed,
				     new double_fclose (*this, arg));
	    sm_ctxt->on_transition (node, stmt, arg, m_closed, m_stop);
	    return true;
	  }

	if (is_file_using_fn_p (callee_fndecl))
	  {
	    // TODO: operations on unchecked file
	    return true;
	  }
	// etc
      }

  return false;
}

/* Implementation of state_machine::on_condition vfunc for
   fileptr_state_machine.
   Potentially transition state 'unchecked' to 'nonnull' or to 'null'.  */

void
fileptr_state_machine::on_condition (sm_context *sm_ctxt,
				     const supernode *node,
				     const gimple *stmt,
				     tree lhs,
				     enum tree_code op,
				     tree rhs) const
{
  if (!zerop (rhs))
    return;

  // TODO: has to be a FILE *, specifically
  if (TREE_CODE (TREE_TYPE (lhs)) != POINTER_TYPE)
    return;

  // TODO: has to be a FILE *, specifically
  if (TREE_CODE (TREE_TYPE (rhs)) != POINTER_TYPE)
    return;

  if (op == NE_EXPR)
    {
      log ("got 'ARG != 0' match");
      sm_ctxt->on_transition (node, stmt,
			      lhs, m_unchecked, m_nonnull);
    }
  else if (op == EQ_EXPR)
    {
      log ("got 'ARG == 0' match");
      sm_ctxt->on_transition (node, stmt,
			      lhs, m_unchecked, m_null);
    }
}

/* Implementation of state_machine::can_purge_p vfunc for fileptr_state_machine.
   Don't allow purging of pointers in state 'unchecked' or 'nonnull'
   (to avoid false leak reports).  */

bool
fileptr_state_machine::can_purge_p (state_t s) const
{
  return s != m_unchecked && s != m_nonnull;
}

/* Implementation of state_machine::on_leak vfunc for
   fileptr_state_machine, for complaining about leaks of FILE * in
   state 'unchecked' and 'nonnull'.  */

pending_diagnostic *
fileptr_state_machine::on_leak (tree var) const
{
  return new file_leak (*this, var);
}

} // anonymous namespace

/* Internal interface to this file. */

state_machine *
make_fileptr_state_machine (logger *logger)
{
  return new fileptr_state_machine (logger);
}

#if CHECKING_P

namespace selftest {

/* Run all of the selftests within this file.  */

void
analyzer_sm_file_cc_tests ()
{
  function_set fs = get_file_using_fns ();
  fs.assert_sorted ();
  fs.assert_sane ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
