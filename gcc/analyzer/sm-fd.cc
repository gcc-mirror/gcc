/* A state machine for detecting misuses of POSIX file descriptor APIs.
   Copyright (C) 2019-2022 Free Software Foundation, Inc.
   Contributed by Immad Mir <mir@sourceware.org>.

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
#include "json.h"
#include "analyzer/analyzer.h"
#include "diagnostic-event-id.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/sm.h"
#include "analyzer/pending-diagnostic.h"
#include "analyzer/function-set.h"
#include "analyzer/analyzer-selftests.h"
#include "tristate.h"
#include "selftest.h"
#include "stringpool.h"
#include "attribs.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "bitmap.h"

#if ENABLE_ANALYZER

namespace ana {

namespace {

/* An enum for distinguishing between three different access modes.  */

enum access_mode
{
  READ_WRITE,
  READ_ONLY,
  WRITE_ONLY
};

enum access_directions
{
  DIRS_READ_WRITE,
  DIRS_READ,
  DIRS_WRITE
};

/* An enum for distinguishing between dup, dup2 and dup3.  */
enum dup
{
  DUP_1,
  DUP_2,
  DUP_3
};

class fd_state_machine : public state_machine
{
public:
  fd_state_machine (logger *logger);

  bool
  inherited_state_p () const final override
  {
    return false;
  }

  state_machine::state_t
  get_default_state (const svalue *sval) const final override
  {
    if (tree cst = sval->maybe_get_constant ())
      {
	if (TREE_CODE (cst) == INTEGER_CST)
	  {
	    int val = TREE_INT_CST_LOW (cst);
	    if (val >= 0)
	      return m_constant_fd;
	    else
	      return m_invalid;
	  }
      }
    return m_start;
  }

  bool on_stmt (sm_context *sm_ctxt, const supernode *node,
		const gimple *stmt) const final override;

  void on_condition (sm_context *sm_ctxt, const supernode *node,
		     const gimple *stmt, const svalue *lhs, const tree_code op,
		     const svalue *rhs) const final override;

  bool can_purge_p (state_t s) const final override;
  pending_diagnostic *on_leak (tree var) const final override;

  bool is_unchecked_fd_p (state_t s) const;
  bool is_valid_fd_p (state_t s) const;
  bool is_closed_fd_p (state_t s) const;
  bool is_constant_fd_p (state_t s) const;
  bool is_readonly_fd_p (state_t s) const;
  bool is_writeonly_fd_p (state_t s) const;
  enum access_mode get_access_mode_from_flag (int flag) const;
  /* Function for one-to-one correspondence between valid
     and unchecked states.  */
  state_t valid_to_unchecked_state (state_t state) const;
  /* State for a constant file descriptor (>= 0) */
  state_t m_constant_fd;

  /* States representing a file descriptor that hasn't yet been
    checked for validity after opening, for three different
    access modes.  */
  state_t m_unchecked_read_write;

  state_t m_unchecked_read_only;

  state_t m_unchecked_write_only;

  /* States for representing a file descriptor that is known to be valid (>=
    0), for three different access modes.  */
  state_t m_valid_read_write;

  state_t m_valid_read_only;

  state_t m_valid_write_only;

  /* State for a file descriptor that is known to be invalid (< 0). */
  state_t m_invalid;

  /* State for a file descriptor that has been closed.  */
  state_t m_closed;

  /* State for a file descriptor that we do not want to track anymore . */
  state_t m_stop;

private:
  void on_open (sm_context *sm_ctxt, const supernode *node, const gimple *stmt,
		const gcall *call) const;
  void on_creat (sm_context *sm_ctxt, const supernode *node, const gimple *stmt,
		const gcall *call) const;
  void on_close (sm_context *sm_ctxt, const supernode *node, const gimple *stmt,
		 const gcall *call) const;
  void on_read (sm_context *sm_ctxt, const supernode *node, const gimple *stmt,
		const gcall *call, const tree callee_fndecl) const;
  void on_write (sm_context *sm_ctxt, const supernode *node, const gimple *stmt,
		 const gcall *call, const tree callee_fndecl) const;
  void check_for_open_fd (sm_context *sm_ctxt, const supernode *node,
			  const gimple *stmt, const gcall *call,
			  const tree callee_fndecl,
			  enum access_directions access_fn) const;

  void make_valid_transitions_on_condition (sm_context *sm_ctxt,
					    const supernode *node,
					    const gimple *stmt,
					    const svalue *lhs) const;
  void make_invalid_transitions_on_condition (sm_context *sm_ctxt,
					      const supernode *node,
					      const gimple *stmt,
					      const svalue *lhs) const;
  void check_for_fd_attrs (sm_context *sm_ctxt, const supernode *node,
			   const gimple *stmt, const gcall *call,
			   const tree callee_fndecl, const char *attr_name,
			   access_directions fd_attr_access_dir) const;
  void check_for_dup (sm_context *sm_ctxt, const supernode *node,
       const gimple *stmt, const gcall *call, const tree callee_fndecl,
       enum dup kind) const;
};

/* Base diagnostic class relative to fd_state_machine.  */
class fd_diagnostic : public pending_diagnostic
{
public:
  fd_diagnostic (const fd_state_machine &sm, tree arg) : m_sm (sm), m_arg (arg)
  {
  }

  bool
  subclass_equal_p (const pending_diagnostic &base_other) const override
  {
    return same_tree_p (m_arg, ((const fd_diagnostic &)base_other).m_arg);
  }

  label_text
  describe_state_change (const evdesc::state_change &change) override
  {
    if (change.m_old_state == m_sm.get_start_state ()
	&& m_sm.is_unchecked_fd_p (change.m_new_state))
      {
	if (change.m_new_state == m_sm.m_unchecked_read_write)
	  return change.formatted_print ("opened here as read-write");

	if (change.m_new_state == m_sm.m_unchecked_read_only)
	  return change.formatted_print ("opened here as read-only");

	if (change.m_new_state == m_sm.m_unchecked_write_only)
	  return change.formatted_print ("opened here as write-only");
      }

    if (change.m_new_state == m_sm.m_closed)
      return change.formatted_print ("closed here");

    if (m_sm.is_unchecked_fd_p (change.m_old_state)
	&& m_sm.is_valid_fd_p (change.m_new_state))
      {
	if (change.m_expr)
	  return change.formatted_print (
	      "assuming %qE is a valid file descriptor (>= 0)", change.m_expr);
	else
	  return change.formatted_print ("assuming a valid file descriptor");
      }

    if (m_sm.is_unchecked_fd_p (change.m_old_state)
	&& change.m_new_state == m_sm.m_invalid)
      {
	if (change.m_expr)
	  return change.formatted_print (
	      "assuming %qE is an invalid file descriptor (< 0)",
	      change.m_expr);
	else
	  return change.formatted_print ("assuming an invalid file descriptor");
      }

    return label_text ();
  }

  diagnostic_event::meaning
  get_meaning_for_state_change (
      const evdesc::state_change &change) const final override
  {
    if (change.m_old_state == m_sm.get_start_state ()
		&& (m_sm.is_unchecked_fd_p (change.m_new_state)))
      return diagnostic_event::meaning (diagnostic_event::VERB_acquire,
			 diagnostic_event::NOUN_resource);
    if (change.m_new_state == m_sm.m_closed)
      return diagnostic_event::meaning (diagnostic_event::VERB_release,
			 diagnostic_event::NOUN_resource);
    return diagnostic_event::meaning ();
  }

protected:
  const fd_state_machine &m_sm;
  tree m_arg;
};

class fd_param_diagnostic : public fd_diagnostic
{
public:
  fd_param_diagnostic (const fd_state_machine &sm, tree arg, tree callee_fndecl,
		       const char *attr_name, int arg_idx)
      : fd_diagnostic (sm, arg), m_callee_fndecl (callee_fndecl),
	m_attr_name (attr_name), m_arg_idx (arg_idx)
  {
  }

  fd_param_diagnostic (const fd_state_machine &sm, tree arg, tree callee_fndecl)
      : fd_diagnostic (sm, arg), m_callee_fndecl (callee_fndecl),
	m_attr_name (NULL), m_arg_idx (-1)
  {
  }

  bool
  subclass_equal_p (const pending_diagnostic &base_other) const override
  {
    const fd_param_diagnostic &sub_other
	= (const fd_param_diagnostic &)base_other;
    return (same_tree_p (m_arg, sub_other.m_arg)
	    && same_tree_p (m_callee_fndecl, sub_other.m_callee_fndecl)
	    && m_arg_idx == sub_other.m_arg_idx
	    && ((m_attr_name)
		    ? (strcmp (m_attr_name, sub_other.m_attr_name) == 0)
		    : true));
  }

  void
  inform_filedescriptor_attribute (access_directions fd_dir)
  {

    if (m_attr_name)
      switch (fd_dir)
	{
	case DIRS_READ_WRITE:
	  inform (DECL_SOURCE_LOCATION (m_callee_fndecl),
		  "argument %d of %qD must be an open file descriptor, due to "
		  "%<__attribute__((%s(%d)))%>",
		  m_arg_idx + 1, m_callee_fndecl, m_attr_name, m_arg_idx + 1);
	  break;
	case DIRS_WRITE:
	  inform (DECL_SOURCE_LOCATION (m_callee_fndecl),
		  "argument %d of %qD must be a readable file descriptor, due "
		  "to %<__attribute__((%s(%d)))%>",
		  m_arg_idx + 1, m_callee_fndecl, m_attr_name, m_arg_idx + 1);
	  break;
	case DIRS_READ:
	  inform (DECL_SOURCE_LOCATION (m_callee_fndecl),
		  "argument %d of %qD must be a writable file descriptor, due "
		  "to %<__attribute__((%s(%d)))%>",
		  m_arg_idx + 1, m_callee_fndecl, m_attr_name, m_arg_idx + 1);
	  break;
	}
  }

protected:
  tree m_callee_fndecl;
  const char *m_attr_name;
  /* ARG_IDX is 0-based.  */
  int m_arg_idx;
};

class fd_leak : public fd_diagnostic
{
public:
  fd_leak (const fd_state_machine &sm, tree arg) : fd_diagnostic (sm, arg) {}

  const char *
  get_kind () const final override
  {
    return "fd_leak";
  }

  int
  get_controlling_option () const final override
  {
    return OPT_Wanalyzer_fd_leak;
  }

  bool
  emit (rich_location *rich_loc) final override
  {
    /*CWE-775: Missing Release of File Descriptor or Handle after Effective
      Lifetime
     */
    diagnostic_metadata m;
    m.add_cwe (775);
    if (m_arg)
      return warning_meta (rich_loc, m, get_controlling_option (),
			   "leak of file descriptor %qE", m_arg);
    else
      return warning_meta (rich_loc, m, get_controlling_option (),
			   "leak of file descriptor");
  }

  label_text
  describe_state_change (const evdesc::state_change &change) final override
  {
    if (m_sm.is_unchecked_fd_p (change.m_new_state))
      {
	m_open_event = change.m_event_id;
	return label_text::borrow ("opened here");
      }

    return fd_diagnostic::describe_state_change (change);
  }

  label_text
  describe_final_event (const evdesc::final_event &ev) final override
  {
    if (m_open_event.known_p ())
      {
	if (ev.m_expr)
	  return ev.formatted_print ("%qE leaks here; was opened at %@",
				     ev.m_expr, &m_open_event);
	else
	  return ev.formatted_print ("leaks here; was opened at %@",
				     &m_open_event);
      }
    else
      {
	if (ev.m_expr)
	  return ev.formatted_print ("%qE leaks here", ev.m_expr);
	else
	  return ev.formatted_print ("leaks here");
      }
  }

private:
  diagnostic_event_id_t m_open_event;
};

class fd_access_mode_mismatch : public fd_param_diagnostic
{
public:
  fd_access_mode_mismatch (const fd_state_machine &sm, tree arg,
			   enum access_directions fd_dir,
			   const tree callee_fndecl, const char *attr_name,
			   int arg_idx)
      : fd_param_diagnostic (sm, arg, callee_fndecl, attr_name, arg_idx),
	m_fd_dir (fd_dir)

  {
  }

  fd_access_mode_mismatch (const fd_state_machine &sm, tree arg,
			   enum access_directions fd_dir,
			   const tree callee_fndecl)
      : fd_param_diagnostic (sm, arg, callee_fndecl), m_fd_dir (fd_dir)
  {
  }

  const char *
  get_kind () const final override
  {
    return "fd_access_mode_mismatch";
  }

  int
  get_controlling_option () const final override
  {
    return OPT_Wanalyzer_fd_access_mode_mismatch;
  }

  bool
  emit (rich_location *rich_loc) final override
  {
    bool warned;
    switch (m_fd_dir)
      {
      case DIRS_READ:
	warned =  warning_at (rich_loc, get_controlling_option (),
			   "%qE on read-only file descriptor %qE",
			   m_callee_fndecl, m_arg);
	break;
      case DIRS_WRITE:
	warned = warning_at (rich_loc, get_controlling_option (),
			   "%qE on write-only file descriptor %qE",
			   m_callee_fndecl, m_arg);
	break;
      default:
	gcc_unreachable ();
      }
      if (warned)
	inform_filedescriptor_attribute (m_fd_dir);
      return warned;
  }

  label_text
  describe_final_event (const evdesc::final_event &ev) final override
  {
    switch (m_fd_dir)
      {
      case DIRS_READ:
	return ev.formatted_print ("%qE on read-only file descriptor %qE",
				   m_callee_fndecl, m_arg);
      case DIRS_WRITE:
	return ev.formatted_print ("%qE on write-only file descriptor %qE",
				   m_callee_fndecl, m_arg);
      default:
	gcc_unreachable ();
      }
  }

private:
  enum access_directions m_fd_dir;
};

class fd_double_close : public fd_diagnostic
{
public:
  fd_double_close (const fd_state_machine &sm, tree arg) : fd_diagnostic (sm, arg)
  {
  }

  const char *
  get_kind () const final override
  {
    return "fd_double_close";
  }

  int
  get_controlling_option () const final override
  {
    return OPT_Wanalyzer_fd_double_close;
  }
  bool
  emit (rich_location *rich_loc) final override
  {
    diagnostic_metadata m;
    // CWE-1341: Multiple Releases of Same Resource or Handle
    m.add_cwe (1341);
    return warning_meta (rich_loc, m, get_controlling_option (),
			 "double %<close%> of file descriptor %qE", m_arg);
  }

  label_text
  describe_state_change (const evdesc::state_change &change) override
  {
    if (m_sm.is_unchecked_fd_p (change.m_new_state))
      return label_text::borrow ("opened here");

    if (change.m_new_state == m_sm.m_closed)
      {
	m_first_close_event = change.m_event_id;
	return change.formatted_print ("first %qs here", "close");
      }
    return fd_diagnostic::describe_state_change (change);
  }

  label_text
  describe_final_event (const evdesc::final_event &ev) final override
  {
    if (m_first_close_event.known_p ())
      return ev.formatted_print ("second %qs here; first %qs was at %@",
				 "close", "close", &m_first_close_event);
    return ev.formatted_print ("second %qs here", "close");
  }

private:
  diagnostic_event_id_t m_first_close_event;
};

class fd_use_after_close : public fd_param_diagnostic
{
public:
  fd_use_after_close (const fd_state_machine &sm, tree arg,
		      const tree callee_fndecl, const char *attr_name,
		      int arg_idx)
      : fd_param_diagnostic (sm, arg, callee_fndecl, attr_name, arg_idx)
  {
  }

  fd_use_after_close (const fd_state_machine &sm, tree arg,
		      const tree callee_fndecl)
      : fd_param_diagnostic (sm, arg, callee_fndecl)
  {
  }

  const char *
  get_kind () const final override
  {
    return "fd_use_after_close";
  }

  int
  get_controlling_option () const final override
  {
    return OPT_Wanalyzer_fd_use_after_close;
  }

  bool
  emit (rich_location *rich_loc) final override
  {
    bool warned;
    warned = warning_at (rich_loc, get_controlling_option (),
		       "%qE on closed file descriptor %qE", m_callee_fndecl,
		       m_arg);
    if (warned)
      inform_filedescriptor_attribute (DIRS_READ_WRITE);
    return warned;
  }

  label_text
  describe_state_change (const evdesc::state_change &change) override
  {
    if (m_sm.is_unchecked_fd_p (change.m_new_state))
      return label_text::borrow ("opened here");

    if (change.m_new_state == m_sm.m_closed)
      {
	m_first_close_event = change.m_event_id;
	return change.formatted_print ("closed here");
      }

    return fd_diagnostic::describe_state_change (change);
  }

  label_text
  describe_final_event (const evdesc::final_event &ev) final override
  {
    if (m_first_close_event.known_p ())
	return ev.formatted_print (
	    "%qE on closed file descriptor %qE; %qs was at %@", m_callee_fndecl,
	    m_arg, "close", &m_first_close_event);
      else
	return ev.formatted_print ("%qE on closed file descriptor %qE",
				  m_callee_fndecl, m_arg);
  }

private:
  diagnostic_event_id_t m_first_close_event;
};

class fd_use_without_check : public fd_param_diagnostic
{
public:
  fd_use_without_check (const fd_state_machine &sm, tree arg,
			const tree callee_fndecl, const char *attr_name,
			int arg_idx)
      : fd_param_diagnostic (sm, arg, callee_fndecl, attr_name, arg_idx)
  {
  }

  fd_use_without_check (const fd_state_machine &sm, tree arg,
			const tree callee_fndecl)
      : fd_param_diagnostic (sm, arg, callee_fndecl)
  {
  }

  const char *
  get_kind () const final override
  {
    return "fd_use_without_check";
  }

  int
  get_controlling_option () const final override
  {
    return OPT_Wanalyzer_fd_use_without_check;
  }

  bool
  emit (rich_location *rich_loc) final override
  {
    bool warned;
    warned = warning_at (rich_loc, get_controlling_option (),
			"%qE on possibly invalid file descriptor %qE",
			m_callee_fndecl, m_arg);
    if (warned)
     inform_filedescriptor_attribute (DIRS_READ_WRITE);
    return warned;
  }

  label_text
  describe_state_change (const evdesc::state_change &change) override
  {
    if (m_sm.is_unchecked_fd_p (change.m_new_state))
      {
	m_first_open_event = change.m_event_id;
	return label_text::borrow ("opened here");
      }

    return fd_diagnostic::describe_state_change (change);
  }

  label_text
  describe_final_event (const evdesc::final_event &ev) final override
  {
    if (m_first_open_event.known_p ())
      return ev.formatted_print (
	  "%qE could be invalid: unchecked value from %@", m_arg,
	  &m_first_open_event);
    else
      return ev.formatted_print ("%qE could be invalid", m_arg);
  }

private:
  diagnostic_event_id_t m_first_open_event;
};

fd_state_machine::fd_state_machine (logger *logger)
    : state_machine ("file-descriptor", logger),
      m_constant_fd (add_state ("fd-constant")),
      m_unchecked_read_write (add_state ("fd-unchecked-read-write")),
      m_unchecked_read_only (add_state ("fd-unchecked-read-only")),
      m_unchecked_write_only (add_state ("fd-unchecked-write-only")),
      m_valid_read_write (add_state ("fd-valid-read-write")),
      m_valid_read_only (add_state ("fd-valid-read-only")),
      m_valid_write_only (add_state ("fd-valid-write-only")),
      m_invalid (add_state ("fd-invalid")),
      m_closed (add_state ("fd-closed")),
      m_stop (add_state ("fd-stop"))
{
}

bool
fd_state_machine::is_unchecked_fd_p (state_t s) const
{
  return (s == m_unchecked_read_write
       || s == m_unchecked_read_only
       || s == m_unchecked_write_only);
}

bool
fd_state_machine::is_valid_fd_p (state_t s) const
{
  return (s == m_valid_read_write
       || s == m_valid_read_only
       || s == m_valid_write_only);
}

enum access_mode
fd_state_machine::get_access_mode_from_flag (int flag) const
{
  /* FIXME: this code assumes the access modes on the host and
     target are the same, which in practice might not be the case.  */

  if ((flag & O_ACCMODE) == O_RDONLY)
    {
      return READ_ONLY;
    }
  else if ((flag & O_ACCMODE) == O_WRONLY)
    {
      return WRITE_ONLY;
    }
  return READ_WRITE;
}

bool
fd_state_machine::is_readonly_fd_p (state_t state) const
{
  return (state == m_unchecked_read_only || state == m_valid_read_only);
}

bool
fd_state_machine::is_writeonly_fd_p (state_t state) const
{
  return (state == m_unchecked_write_only || state == m_valid_write_only);
}

bool
fd_state_machine::is_closed_fd_p (state_t state) const
{
  return (state == m_closed);
}

bool
fd_state_machine::is_constant_fd_p (state_t state) const
{
  return (state == m_constant_fd);
}

fd_state_machine::state_t
fd_state_machine::valid_to_unchecked_state (state_t state) const
{
  if (state == m_valid_read_write)
    return m_unchecked_read_write;
  else if (state == m_valid_write_only)
    return m_unchecked_write_only;
  else if (state == m_valid_read_only)
    return m_unchecked_read_only;
  else
    gcc_unreachable ();
  return NULL;
}

bool
fd_state_machine::on_stmt (sm_context *sm_ctxt, const supernode *node,
			   const gimple *stmt) const
{
  if (const gcall *call = dyn_cast<const gcall *> (stmt))
    if (tree callee_fndecl = sm_ctxt->get_fndecl_for_call (call))
      {
	if (is_named_call_p (callee_fndecl, "open", call, 2))
	  {
	    on_open (sm_ctxt, node, stmt, call);
	    return true;
	  } //  "open"

	if (is_named_call_p (callee_fndecl, "creat", call, 2))
	  {
	    on_creat (sm_ctxt, node, stmt, call);
	  } // "creat"

	if (is_named_call_p (callee_fndecl, "close", call, 1))
	  {
	    on_close (sm_ctxt, node, stmt, call);
	    return true;
	  } //  "close"

	if (is_named_call_p (callee_fndecl, "write", call, 3))
	  {
	    on_write (sm_ctxt, node, stmt, call, callee_fndecl);
	    return true;
	  } // "write"

	if (is_named_call_p (callee_fndecl, "read", call, 3))
	  {
	    on_read (sm_ctxt, node, stmt, call, callee_fndecl);
	    return true;
	  } // "read"

	if (is_named_call_p (callee_fndecl, "dup", call, 1))
	  {
	    check_for_dup (sm_ctxt, node, stmt, call, callee_fndecl, DUP_1);
	    return true;
	  }

	if (is_named_call_p (callee_fndecl, "dup2", call, 2))
	  {
	    check_for_dup (sm_ctxt, node, stmt, call, callee_fndecl, DUP_2);
	    return true;
	  }

	if (is_named_call_p (callee_fndecl, "dup3", call, 3))
	  {
	    check_for_dup (sm_ctxt, node, stmt, call, callee_fndecl, DUP_3);
	    return true;
	  }

	{
	  // Handle __attribute__((fd_arg))

	  check_for_fd_attrs (sm_ctxt, node, stmt, call, callee_fndecl,
			      "fd_arg", DIRS_READ_WRITE);

	  // Handle __attribute__((fd_arg_read))

	  check_for_fd_attrs (sm_ctxt, node, stmt, call, callee_fndecl,
			      "fd_arg_read", DIRS_READ);

	  // Handle __attribute__((fd_arg_write))

	  check_for_fd_attrs (sm_ctxt, node, stmt, call, callee_fndecl,
			      "fd_arg_write", DIRS_WRITE);
	}
      }

  return false;
}

void
fd_state_machine::check_for_fd_attrs (
    sm_context *sm_ctxt, const supernode *node, const gimple *stmt,
    const gcall *call, const tree callee_fndecl, const char *attr_name,
    access_directions fd_attr_access_dir) const
{

  tree attrs = TYPE_ATTRIBUTES (TREE_TYPE (callee_fndecl));
  attrs = lookup_attribute (attr_name, attrs);
  if (!attrs)
    return;

  if (!TREE_VALUE (attrs))
    return;

  auto_bitmap argmap;

  for (tree idx = TREE_VALUE (attrs); idx; idx = TREE_CHAIN (idx))
    {
      unsigned int val = TREE_INT_CST_LOW (TREE_VALUE (idx)) - 1;
      bitmap_set_bit (argmap, val);
    }
  if (bitmap_empty_p (argmap))
    return;

  for (unsigned arg_idx = 0; arg_idx < gimple_call_num_args (call); arg_idx++)
    {
      tree arg = gimple_call_arg (call, arg_idx);
      tree diag_arg = sm_ctxt->get_diagnostic_tree (arg);
      state_t state = sm_ctxt->get_state (stmt, arg);
      bool bit_set = bitmap_bit_p (argmap, arg_idx);
      if (TREE_CODE (TREE_TYPE (arg)) != INTEGER_TYPE)
	continue;
      if (bit_set) // Check if arg_idx is marked by any of the file descriptor
		   // attributes
	{

	  if (is_closed_fd_p (state))
	    {

	      sm_ctxt->warn (node, stmt, arg,
			     new fd_use_after_close (*this, diag_arg,
						     callee_fndecl, attr_name,
						     arg_idx));
	      continue;
	    }

	  if (!(is_valid_fd_p (state) || (state == m_stop)))
	    {
	      if (!is_constant_fd_p (state))
		sm_ctxt->warn (node, stmt, arg,
			       new fd_use_without_check (*this, diag_arg,
							callee_fndecl, attr_name,
							arg_idx));
	    }

	  switch (fd_attr_access_dir)
	    {
	    case DIRS_READ_WRITE:
	      break;
	    case DIRS_READ:

	      if (is_writeonly_fd_p (state))
		{
		  sm_ctxt->warn (
		      node, stmt, arg,
		      new fd_access_mode_mismatch (*this, diag_arg, DIRS_WRITE,
						   callee_fndecl, attr_name, arg_idx));
		}

	      break;
	    case DIRS_WRITE:

	      if (is_readonly_fd_p (state))
		{
		  sm_ctxt->warn (
		      node, stmt, arg,
		      new fd_access_mode_mismatch (*this, diag_arg, DIRS_READ,
						   callee_fndecl, attr_name, arg_idx));
		}

	      break;
	    }
	}
    }
}


void
fd_state_machine::on_open (sm_context *sm_ctxt, const supernode *node,
			   const gimple *stmt, const gcall *call) const
{
  tree lhs = gimple_call_lhs (call);
  if (lhs)
    {
      tree arg = gimple_call_arg (call, 1);
      if (TREE_CODE (arg) == INTEGER_CST)
	{
	  int flag = TREE_INT_CST_LOW (arg);
	  enum access_mode mode = get_access_mode_from_flag (flag);

	  switch (mode)
	    {
	    case READ_ONLY:
	      sm_ctxt->on_transition (node, stmt, lhs, m_start,
				      m_unchecked_read_only);
	      break;
	    case WRITE_ONLY:
	      sm_ctxt->on_transition (node, stmt, lhs, m_start,
				      m_unchecked_write_only);
	      break;
	    default:
	      sm_ctxt->on_transition (node, stmt, lhs, m_start,
				      m_unchecked_read_write);
	    }
	}
    }
  else
    {
      sm_ctxt->warn (node, stmt, NULL_TREE, new fd_leak (*this, NULL_TREE));
    }
}

void
fd_state_machine::on_creat (sm_context *sm_ctxt, const supernode *node,
			    const gimple *stmt, const gcall *call) const
{
  tree lhs = gimple_call_lhs (call);
  if (lhs)
    sm_ctxt->on_transition (node, stmt, lhs, m_start, m_unchecked_write_only);
  else
    sm_ctxt->warn (node, stmt, NULL_TREE, new fd_leak (*this, NULL_TREE));
}

void
fd_state_machine::check_for_dup (sm_context *sm_ctxt, const supernode *node,
				 const gimple *stmt, const gcall *call,
				 const tree callee_fndecl, enum dup kind) const
{
  tree lhs = gimple_call_lhs (call);
  tree arg_1 = gimple_call_arg (call, 0);
  state_t state_arg_1 = sm_ctxt->get_state (stmt, arg_1);
  if (state_arg_1 == m_stop)
    return;
  if (!(is_constant_fd_p (state_arg_1) || is_valid_fd_p (state_arg_1)
	|| state_arg_1 == m_start))
    {
      check_for_open_fd (sm_ctxt, node, stmt, call, callee_fndecl,
			 DIRS_READ_WRITE);
      return;
    }
  switch (kind)
    {
    case DUP_1:
      if (lhs)
	{
	  if (is_constant_fd_p (state_arg_1) || state_arg_1 == m_start)
	    sm_ctxt->set_next_state (stmt, lhs, m_unchecked_read_write);
	  else
	    sm_ctxt->set_next_state (stmt, lhs,
				     valid_to_unchecked_state (state_arg_1));
	}
      break;

    case DUP_2:
    case DUP_3:
      tree arg_2 = gimple_call_arg (call, 1);
      state_t state_arg_2 = sm_ctxt->get_state (stmt, arg_2);
      tree diag_arg_2 = sm_ctxt->get_diagnostic_tree (arg_2);
      if (state_arg_2 == m_stop)
	return;
      /* Check if -1 was passed as second argument to dup2.  */
      if (!(is_constant_fd_p (state_arg_2) || is_valid_fd_p (state_arg_2)
	    || state_arg_2 == m_start))
	{
	  sm_ctxt->warn (
	      node, stmt, arg_2,
	      new fd_use_without_check (*this, diag_arg_2, callee_fndecl));
	  return;
	}
      /* dup2 returns value of its second argument on success.But, the
      access mode of the returned file descriptor depends on the duplicated
      file descriptor i.e the first argument.  */
      if (lhs)
	{
	  if (is_constant_fd_p (state_arg_1) || state_arg_1 == m_start)
	    sm_ctxt->set_next_state (stmt, lhs, m_unchecked_read_write);
	  else
	    sm_ctxt->set_next_state (stmt, lhs,
				     valid_to_unchecked_state (state_arg_1));
	}

      break;
    }
}

void
fd_state_machine::on_close (sm_context *sm_ctxt, const supernode *node,
			    const gimple *stmt, const gcall *call) const
{
  tree arg = gimple_call_arg (call, 0);
  state_t state = sm_ctxt->get_state (stmt, arg);
  tree diag_arg = sm_ctxt->get_diagnostic_tree (arg);

  sm_ctxt->on_transition (node, stmt, arg, m_start, m_closed);
  sm_ctxt->on_transition (node, stmt, arg, m_unchecked_read_write, m_closed);
  sm_ctxt->on_transition (node, stmt, arg, m_unchecked_read_only, m_closed);
  sm_ctxt->on_transition (node, stmt, arg, m_unchecked_write_only, m_closed);
  sm_ctxt->on_transition (node, stmt, arg, m_valid_read_write, m_closed);
  sm_ctxt->on_transition (node, stmt, arg, m_valid_read_only, m_closed);
  sm_ctxt->on_transition (node, stmt, arg, m_valid_write_only, m_closed);
  sm_ctxt->on_transition (node, stmt, arg, m_constant_fd, m_closed);

  if (is_closed_fd_p (state))
    {
      sm_ctxt->warn (node, stmt, arg, new fd_double_close (*this, diag_arg));
      sm_ctxt->set_next_state (stmt, arg, m_stop);
    }
}
void
fd_state_machine::on_read (sm_context *sm_ctxt, const supernode *node,
			   const gimple *stmt, const gcall *call,
			   const tree callee_fndecl) const
{
  check_for_open_fd (sm_ctxt, node, stmt, call, callee_fndecl, DIRS_READ);
}
void
fd_state_machine::on_write (sm_context *sm_ctxt, const supernode *node,
			    const gimple *stmt, const gcall *call,
			    const tree callee_fndecl) const
{
  check_for_open_fd (sm_ctxt, node, stmt, call, callee_fndecl, DIRS_WRITE);
}

void
fd_state_machine::check_for_open_fd (
    sm_context *sm_ctxt, const supernode *node, const gimple *stmt,
    const gcall *call, const tree callee_fndecl,
    enum access_directions callee_fndecl_dir) const
{
  tree arg = gimple_call_arg (call, 0);
  tree diag_arg = sm_ctxt->get_diagnostic_tree (arg);
  state_t state = sm_ctxt->get_state (stmt, arg);

  if (is_closed_fd_p (state))
    {
      sm_ctxt->warn (node, stmt, arg,
		     new fd_use_after_close (*this, diag_arg, callee_fndecl));
    }

  else
    {
      if (!(is_valid_fd_p (state) || (state == m_stop)))
	{
	  if (!is_constant_fd_p (state))
	    sm_ctxt->warn (
		node, stmt, arg,
		new fd_use_without_check (*this, diag_arg, callee_fndecl));
	}
      switch (callee_fndecl_dir)
	{
	case DIRS_READ_WRITE:
	  break;
	case DIRS_READ:
	  if (is_writeonly_fd_p (state))
	    {
	      tree diag_arg = sm_ctxt->get_diagnostic_tree (arg);
	      sm_ctxt->warn (node, stmt, arg,
			     new fd_access_mode_mismatch (
				 *this, diag_arg, DIRS_WRITE, callee_fndecl));
	    }

	  break;
	case DIRS_WRITE:

	  if (is_readonly_fd_p (state))
	    {
	      tree diag_arg = sm_ctxt->get_diagnostic_tree (arg);
	      sm_ctxt->warn (node, stmt, arg,
			     new fd_access_mode_mismatch (
				 *this, diag_arg, DIRS_READ, callee_fndecl));
	    }
	  break;
	}
    }
}

void
fd_state_machine::on_condition (sm_context *sm_ctxt, const supernode *node,
				const gimple *stmt, const svalue *lhs,
				enum tree_code op, const svalue *rhs) const
{
  if (tree cst = rhs->maybe_get_constant ())
    {
      if (TREE_CODE (cst) == INTEGER_CST)
	{
	  int val = TREE_INT_CST_LOW (cst);
	  if (val == -1)
	    {
	      if (op == NE_EXPR)
		make_valid_transitions_on_condition (sm_ctxt, node, stmt, lhs);

	      else if (op == EQ_EXPR)
		make_invalid_transitions_on_condition (sm_ctxt, node, stmt,
						       lhs);
	    }
	}
    }

  if (rhs->all_zeroes_p ())
    {
      if (op == GE_EXPR)
	make_valid_transitions_on_condition (sm_ctxt, node, stmt, lhs);
      else if (op == LT_EXPR)
	make_invalid_transitions_on_condition (sm_ctxt, node, stmt, lhs);
    }
}

void
fd_state_machine::make_valid_transitions_on_condition (sm_context *sm_ctxt,
						       const supernode *node,
						       const gimple *stmt,
						       const svalue *lhs) const
{
  sm_ctxt->on_transition (node, stmt, lhs, m_unchecked_read_write,
			  m_valid_read_write);
  sm_ctxt->on_transition (node, stmt, lhs, m_unchecked_read_only,
			  m_valid_read_only);
  sm_ctxt->on_transition (node, stmt, lhs, m_unchecked_write_only,
			  m_valid_write_only);
}

void
fd_state_machine::make_invalid_transitions_on_condition (
    sm_context *sm_ctxt, const supernode *node, const gimple *stmt,
    const svalue *lhs) const
{
  sm_ctxt->on_transition (node, stmt, lhs, m_unchecked_read_write, m_invalid);
  sm_ctxt->on_transition (node, stmt, lhs, m_unchecked_read_only, m_invalid);
  sm_ctxt->on_transition (node, stmt, lhs, m_unchecked_write_only, m_invalid);
}

bool
fd_state_machine::can_purge_p (state_t s) const
{
  if (is_unchecked_fd_p (s) || is_valid_fd_p (s))
    return false;
  else
    return true;
}

pending_diagnostic *
fd_state_machine::on_leak (tree var) const
{
  return new fd_leak (*this, var);
}
} // namespace

state_machine *
make_fd_state_machine (logger *logger)
{
  return new fd_state_machine (logger);
}
} // namespace ana

#endif // ENABLE_ANALYZER
