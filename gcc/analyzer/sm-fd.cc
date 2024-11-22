/* A state machine for detecting misuses of POSIX file descriptor APIs.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
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
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "make-unique.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "options.h"
#include "diagnostic-core.h"
#include "diagnostic-path.h"
#include "analyzer/analyzer.h"
#include "diagnostic-event-id.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/sm.h"
#include "analyzer/pending-diagnostic.h"
#include "analyzer/function-set.h"
#include "analyzer/analyzer-selftests.h"
#include "stringpool.h"
#include "attribs.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "bitmap.h"
#include "analyzer/program-state.h"
#include "analyzer/supergraph.h"
#include "analyzer/analyzer-language.h"
#include "analyzer/call-details.h"
#include "analyzer/call-info.h"

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

/* Enum for use by -Wanalyzer-fd-phase-mismatch.  */

enum expected_phase
{
  EXPECTED_PHASE_CAN_TRANSFER, /* can "read"/"write".  */
  EXPECTED_PHASE_CAN_BIND,
  EXPECTED_PHASE_CAN_LISTEN,
  EXPECTED_PHASE_CAN_ACCEPT,
  EXPECTED_PHASE_CAN_CONNECT
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

  bool on_stmt (sm_context &sm_ctxt, const supernode *node,
		const gimple *stmt) const final override;

  void on_condition (sm_context &sm_ctxt, const supernode *node,
		     const gimple *stmt, const svalue *lhs, const tree_code op,
		     const svalue *rhs) const final override;

  bool can_purge_p (state_t s) const final override;
  std::unique_ptr<pending_diagnostic> on_leak (tree var) const final override;

  bool is_unchecked_fd_p (state_t s) const;
  bool is_valid_fd_p (state_t s) const;
  bool is_socket_fd_p (state_t s) const;
  bool is_datagram_socket_fd_p (state_t s) const;
  bool is_stream_socket_fd_p (state_t s) const;
  bool is_closed_fd_p (state_t s) const;
  bool is_constant_fd_p (state_t s) const;
  bool is_readonly_fd_p (state_t s) const;
  bool is_writeonly_fd_p (state_t s) const;
  enum access_mode get_access_mode_from_flag (int flag) const;
  /* Function for one-to-one correspondence between valid
     and unchecked states.  */
  state_t valid_to_unchecked_state (state_t state) const;

  void mark_as_valid_fd (region_model *model,
			 sm_state_map *smap,
			 const svalue *fd_sval,
			 const extrinsic_state &ext_state) const;

  bool on_socket (const call_details &cd,
		  bool successful,
		  sm_context &sm_ctxt,
		  const extrinsic_state &ext_state) const;
  bool on_bind (const call_details &cd,
		bool successful,
		sm_context &sm_ctxt,
		const extrinsic_state &ext_state) const;
  bool on_listen (const call_details &cd,
		  bool successful,
		  sm_context &sm_ctxt,
		  const extrinsic_state &ext_state) const;
  bool on_accept (const call_details &cd,
		  bool successful,
		  sm_context &sm_ctxt,
		  const extrinsic_state &ext_state) const;
  bool on_connect (const call_details &cd,
		   bool successful,
		   sm_context &sm_ctxt,
		   const extrinsic_state &ext_state) const;

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

  /* States for FDs relating to socket APIs.  */

  /* Result of successful "socket" with SOCK_DGRAM.  */
  state_t m_new_datagram_socket;
  /* Result of successful "socket" with SOCK_STREAM.  */
  state_t m_new_stream_socket;
  /* Result of successful "socket" with unknown type.  */
  state_t m_new_unknown_socket;

  /* The above after a successful call to "bind".  */
  state_t m_bound_datagram_socket;
  state_t m_bound_stream_socket;
  state_t m_bound_unknown_socket;

  /* A bound socket after a successful call to "listen" (stream or unknown).  */
  state_t m_listening_stream_socket;

  /* (i) the new FD as a result of a succesful call to "accept" on a
     listening socket (via a passive open), or
     (ii) an active socket after a successful call to "connect"
     (via an active open).  */
  state_t m_connected_stream_socket;

  /* State for a file descriptor that we do not want to track anymore . */
  state_t m_stop;

  /* Stashed constant values from the frontend.  These could be NULL.  */
  tree m_O_ACCMODE;
  tree m_O_RDONLY;
  tree m_O_WRONLY;
  tree m_SOCK_STREAM;
  tree m_SOCK_DGRAM;

private:
  void on_open (sm_context &sm_ctxt, const supernode *node, const gimple *stmt,
		const gcall *call) const;
  void on_creat (sm_context &sm_ctxt, const supernode *node, const gimple *stmt,
		const gcall *call) const;
  void on_close (sm_context &sm_ctxt, const supernode *node, const gimple *stmt,
		 const gcall *call) const;
  void on_read (sm_context &sm_ctxt, const supernode *node, const gimple *stmt,
		const gcall *call, const tree callee_fndecl) const;
  void on_write (sm_context &sm_ctxt, const supernode *node, const gimple *stmt,
		 const gcall *call, const tree callee_fndecl) const;
  void check_for_open_fd (sm_context &sm_ctxt, const supernode *node,
			  const gimple *stmt, const gcall *call,
			  const tree callee_fndecl,
			  enum access_directions access_fn) const;

  void make_valid_transitions_on_condition (sm_context &sm_ctxt,
					    const supernode *node,
					    const gimple *stmt,
					    const svalue *lhs) const;
  void make_invalid_transitions_on_condition (sm_context &sm_ctxt,
					      const supernode *node,
					      const gimple *stmt,
					      const svalue *lhs) const;
  void check_for_fd_attrs (sm_context &sm_ctxt, const supernode *node,
			   const gimple *stmt, const gcall *call,
			   const tree callee_fndecl, const char *attr_name,
			   access_directions fd_attr_access_dir) const;
  void check_for_dup (sm_context &sm_ctxt, const supernode *node,
       const gimple *stmt, const gcall *call, const tree callee_fndecl,
       enum dup kind) const;

  state_t get_state_for_socket_type (const svalue *socket_type_sval) const;

  bool check_for_socket_fd (const call_details &cd,
			    bool successful,
			    sm_context &sm_ctxt,
			    const svalue *fd_sval,
			    const supernode *node,
			    state_t old_state,
			    bool *complained = NULL) const;
  bool check_for_new_socket_fd (const call_details &cd,
				bool successful,
				sm_context &sm_ctxt,
				const svalue *fd_sval,
				const supernode *node,
				state_t old_state,
				enum expected_phase expected_phase) const;
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

  bool
  describe_state_change (pretty_printer &pp,
			 const evdesc::state_change &change) override
  {
    if (change.m_old_state == m_sm.get_start_state ())
      {
	if (change.m_new_state == m_sm.m_unchecked_read_write
	    || change.m_new_state == m_sm.m_valid_read_write)
	  {
	    pp_string (&pp, "opened here as read-write");
	    return true;
	  }

	if (change.m_new_state == m_sm.m_unchecked_read_only
	    || change.m_new_state == m_sm.m_valid_read_only)
	  {
	    pp_string (&pp, "opened here as read-only");
	    return true;
	  }

	if (change.m_new_state == m_sm.m_unchecked_write_only
	    || change.m_new_state == m_sm.m_valid_write_only)
	  {
	    pp_string (&pp, "opened here as write-only");
	    return true;
	  }

	if (change.m_new_state == m_sm.m_new_datagram_socket)
	  {
	    pp_string (&pp, "datagram socket created here");
	    return true;
	  }

	if (change.m_new_state == m_sm.m_new_stream_socket)
	  {
	    pp_string (&pp, "stream socket created here");
	    return true;
	  }

	if (change.m_new_state == m_sm.m_new_unknown_socket
	    || change.m_new_state == m_sm.m_connected_stream_socket)
	  {
	    pp_string (&pp, "socket created here");
	    return true;
	  }
      }

    if (change.m_new_state == m_sm.m_bound_datagram_socket)
      {
	pp_string (&pp, "datagram socket bound here");
	return true;
      }

    if (change.m_new_state == m_sm.m_bound_stream_socket)
      {
	pp_string (&pp, "stream socket bound here");
	return true;
      }

    if (change.m_new_state == m_sm.m_bound_unknown_socket
	|| change.m_new_state == m_sm.m_connected_stream_socket)
      {
	pp_string (&pp, "socket bound here");
	return true;
      }

    if (change.m_new_state == m_sm.m_listening_stream_socket)
      {
	pp_printf (&pp,
		   "stream socket marked as passive here via %qs",
		   "listen");
	return true;
      }

    if (change.m_new_state == m_sm.m_closed)
      {
	pp_string (&pp, "closed here");
	return true;
      }

    if (m_sm.is_unchecked_fd_p (change.m_old_state)
	&& m_sm.is_valid_fd_p (change.m_new_state))
      {
	if (change.m_expr)
	  pp_printf (&pp,
		     "assuming %qE is a valid file descriptor (>= 0)",
		     change.m_expr);
	else
	  pp_string (&pp, "assuming a valid file descriptor");
	return true;
      }

    if (m_sm.is_unchecked_fd_p (change.m_old_state)
	&& change.m_new_state == m_sm.m_invalid)
      {
	if (change.m_expr)
	  pp_printf (&pp,
		     "assuming %qE is an invalid file descriptor (< 0)",
		     change.m_expr);
	else
	  pp_string (&pp, "assuming an invalid file descriptor");
	return true;
      }

    return false;
  }

  diagnostic_event::meaning
  get_meaning_for_state_change (
      const evdesc::state_change &change) const final override
  {
    if (change.m_old_state == m_sm.get_start_state ()
	&& (m_sm.is_unchecked_fd_p (change.m_new_state)
	    || change.m_new_state == m_sm.m_new_datagram_socket
	    || change.m_new_state == m_sm.m_new_stream_socket
	    || change.m_new_state == m_sm.m_new_unknown_socket))
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
  emit (diagnostic_emission_context &ctxt) final override
  {
    /*CWE-775: Missing Release of File Descriptor or Handle after Effective
      Lifetime
     */
    ctxt.add_cwe (775);
    if (m_arg)
      return ctxt.warn ("leak of file descriptor %qE", m_arg);
    else
      return ctxt.warn ("leak of file descriptor");
  }

  bool
  describe_state_change (pretty_printer &pp,
			 const evdesc::state_change &change) final override
  {
    if (m_sm.is_unchecked_fd_p (change.m_new_state))
      {
	m_open_event = change.m_event_id;
	pp_string (&pp, "opened here");
	return true;
      }

    return fd_diagnostic::describe_state_change (pp, change);
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &ev) final override
  {
    if (m_open_event.known_p ())
      {
	if (ev.m_expr)
	  pp_printf (&pp,
		     "%qE leaks here; was opened at %@",
		     ev.m_expr, &m_open_event);
	else
	  pp_printf (&pp,
		     "leaks here; was opened at %@",
		     &m_open_event);
      }
    else
      {
	if (ev.m_expr)
	  pp_printf (&pp, "%qE leaks here", ev.m_expr);
	else
	  pp_string (&pp, "leaks here");
      }
    return true;
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
  emit (diagnostic_emission_context &ctxt) final override
  {
    bool warned;
    switch (m_fd_dir)
      {
      case DIRS_READ:
	warned =  ctxt.warn ("%qE on read-only file descriptor %qE",
			     m_callee_fndecl, m_arg);
	break;
      case DIRS_WRITE:
	warned = ctxt.warn ("%qE on write-only file descriptor %qE",
			    m_callee_fndecl, m_arg);
	break;
      default:
	gcc_unreachable ();
      }
      if (warned)
	inform_filedescriptor_attribute (m_fd_dir);
      return warned;
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    switch (m_fd_dir)
      {
      case DIRS_READ:
	pp_printf (&pp,
		   "%qE on read-only file descriptor %qE",
		   m_callee_fndecl, m_arg);
	return true;
      case DIRS_WRITE:
	pp_printf (&pp,
		   "%qE on write-only file descriptor %qE",
		   m_callee_fndecl, m_arg);
	return true;
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
  emit (diagnostic_emission_context &ctxt) final override
  {
    // CWE-1341: Multiple Releases of Same Resource or Handle
    ctxt.add_cwe (1341);
    return ctxt.warn ("double %<close%> of file descriptor %qE", m_arg);
  }

  bool
  describe_state_change (pretty_printer &pp,
			 const evdesc::state_change &change) override
  {
    if (m_sm.is_unchecked_fd_p (change.m_new_state))
      {
	pp_string (&pp, "opened here");
	return true;
      }

    if (change.m_new_state == m_sm.m_closed)
      {
	m_first_close_event = change.m_event_id;
	pp_printf (&pp, "first %qs here", "close");
	return true;
      }
    return fd_diagnostic::describe_state_change (pp, change);
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    if (m_first_close_event.known_p ())
      pp_printf (&pp,
		 "second %qs here; first %qs was at %@",
		 "close", "close", &m_first_close_event);
    else
      pp_printf (&pp,
		 "second %qs here",
		 "close");
    return true;
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
  emit (diagnostic_emission_context &ctxt) final override
  {
    bool warned = ctxt.warn ("%qE on closed file descriptor %qE",
			     m_callee_fndecl, m_arg);
    if (warned)
      inform_filedescriptor_attribute (DIRS_READ_WRITE);
    return warned;
  }

  bool
  describe_state_change (pretty_printer &pp,
			 const evdesc::state_change &change) override
  {
    if (m_sm.is_unchecked_fd_p (change.m_new_state))
      {
	pp_string (&pp, "opened here");
	return true;
      }

    if (change.m_new_state == m_sm.m_closed)
      {
	m_first_close_event = change.m_event_id;
	pp_string (&pp, "closed here");
	return true;
      }

    return fd_diagnostic::describe_state_change (pp, change);
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    if (m_first_close_event.known_p ())
      pp_printf (&pp,
		 "%qE on closed file descriptor %qE;"
		 " %qs was at %@",
		 m_callee_fndecl, m_arg,
		 "close", &m_first_close_event);
    else
      pp_printf (&pp,
		 "%qE on closed file descriptor %qE",
		 m_callee_fndecl, m_arg);
    return true;
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
  emit (diagnostic_emission_context &ctxt) final override
  {
    bool warned = ctxt.warn ("%qE on possibly invalid file descriptor %qE",
			     m_callee_fndecl, m_arg);
    if (warned)
     inform_filedescriptor_attribute (DIRS_READ_WRITE);
    return warned;
  }

  bool
  describe_state_change (pretty_printer &pp,
			 const evdesc::state_change &change) override
  {
    if (m_sm.is_unchecked_fd_p (change.m_new_state))
      {
	m_first_open_event = change.m_event_id;
	pp_string (&pp, "opened here");
	return true;
      }

    return fd_diagnostic::describe_state_change (pp, change);
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    if (m_first_open_event.known_p ())
      pp_printf (&pp,
		 "%qE could be invalid: unchecked value from %@", m_arg,
		 &m_first_open_event);
    else
      pp_printf (&pp,
		 "%qE could be invalid", m_arg);
    return true;
  }

private:
  diagnostic_event_id_t m_first_open_event;
};

/* Concrete pending_diagnostic subclass for -Wanalyzer-fd-phase-mismatch.  */

class fd_phase_mismatch : public fd_param_diagnostic
{
public:
  fd_phase_mismatch (const fd_state_machine &sm, tree arg,
		     const tree callee_fndecl,
		     state_machine::state_t actual_state,
		     enum expected_phase expected_phase)
  : fd_param_diagnostic (sm, arg, callee_fndecl),
    m_actual_state (actual_state),
    m_expected_phase (expected_phase)
  {
    gcc_assert (m_sm.is_socket_fd_p (actual_state));
    switch (expected_phase)
      {
      case EXPECTED_PHASE_CAN_TRANSFER:
	gcc_assert (actual_state == m_sm.m_new_stream_socket
		    || actual_state == m_sm.m_bound_stream_socket
		    || actual_state == m_sm.m_listening_stream_socket);
	break;
      case EXPECTED_PHASE_CAN_BIND:
	gcc_assert (actual_state == m_sm.m_bound_datagram_socket
		    || actual_state == m_sm.m_bound_stream_socket
		    || actual_state == m_sm.m_bound_unknown_socket
		    || actual_state == m_sm.m_connected_stream_socket
		    || actual_state == m_sm.m_listening_stream_socket);
	break;
      case EXPECTED_PHASE_CAN_LISTEN:
	gcc_assert (actual_state == m_sm.m_new_stream_socket
		    || actual_state == m_sm.m_new_unknown_socket
		    || actual_state == m_sm.m_connected_stream_socket);
	break;
      case EXPECTED_PHASE_CAN_ACCEPT:
	gcc_assert (actual_state == m_sm.m_new_stream_socket
		    || actual_state == m_sm.m_new_unknown_socket
		    || actual_state == m_sm.m_bound_stream_socket
		    || actual_state == m_sm.m_bound_unknown_socket
		    || actual_state == m_sm.m_connected_stream_socket);
	break;
      case EXPECTED_PHASE_CAN_CONNECT:
	gcc_assert (actual_state == m_sm.m_bound_datagram_socket
		    || actual_state == m_sm.m_bound_stream_socket
		    || actual_state == m_sm.m_bound_unknown_socket
		    || actual_state == m_sm.m_listening_stream_socket
		    || actual_state == m_sm.m_connected_stream_socket);
	break;
      }
  }

  const char *
  get_kind () const final override
  {
    return "fd_phase_mismatch";
  }

  bool
  subclass_equal_p (const pending_diagnostic &base_other) const final override
  {
    const fd_phase_mismatch &sub_other = (const fd_phase_mismatch &)base_other;
    if (!fd_param_diagnostic ::subclass_equal_p (sub_other))
      return false;
    return (m_actual_state == sub_other.m_actual_state
	    && m_expected_phase == sub_other.m_expected_phase);
  }

  int
  get_controlling_option () const final override
  {
    return OPT_Wanalyzer_fd_phase_mismatch;
  }

  bool
  emit (diagnostic_emission_context &ctxt) final override
  {
    /* CWE-666: Operation on Resource in Wrong Phase of Lifetime.  */
    ctxt.add_cwe (666);
    return ctxt.warn ("%qE on file descriptor %qE in wrong phase",
		      m_callee_fndecl, m_arg);
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    switch (m_expected_phase)
      {
      case EXPECTED_PHASE_CAN_TRANSFER:
	{
	  if (m_actual_state == m_sm.m_new_stream_socket)
	    {
	      pp_printf (&pp,
			 "%qE expects a stream socket to be connected via %qs"
			 " but %qE has not yet been bound",
			 m_callee_fndecl, "accept", m_arg);
	      return true;
	    }
	  if (m_actual_state == m_sm.m_bound_stream_socket)
	    {
	      pp_printf (&pp,
			 "%qE expects a stream socket to be connected via %qs"
			 " but %qE is not yet listening",
			 m_callee_fndecl, "accept", m_arg);
	      return true;
	    }
	  if (m_actual_state == m_sm.m_listening_stream_socket)
	    {
	      pp_printf (&pp,
			 "%qE expects a stream socket to be connected via"
			 " the return value of %qs"
			 " but %qE is listening; wrong file descriptor?",
			 m_callee_fndecl, "accept", m_arg);
	      return true;
	    }
	}
	break;
      case EXPECTED_PHASE_CAN_BIND:
	{
	  if (m_actual_state == m_sm.m_bound_datagram_socket
	      || m_actual_state == m_sm.m_bound_stream_socket
	      || m_actual_state == m_sm.m_bound_unknown_socket)
	    {
	      pp_printf (&pp,
			 "%qE expects a new socket file descriptor"
			 " but %qE has already been bound",
			 m_callee_fndecl, m_arg);
	      return true;
	    }
	  if (m_actual_state == m_sm.m_connected_stream_socket)
	    {
	      pp_printf (&pp,
			 "%qE expects a new socket file descriptor"
			 " but %qE is already connected",
			 m_callee_fndecl, m_arg);
	      return true;
	    }
	  if (m_actual_state == m_sm.m_listening_stream_socket)
	    {
	      pp_printf (&pp,
			 "%qE expects a new socket file descriptor"
			 " but %qE is already listening",
			 m_callee_fndecl, m_arg);
	      return true;
	    }
	}
	break;
      case EXPECTED_PHASE_CAN_LISTEN:
	{
	  if (m_actual_state == m_sm.m_new_stream_socket
	      || m_actual_state == m_sm.m_new_unknown_socket)
	    {
	      pp_printf (&pp,
			 "%qE expects a bound stream socket file descriptor"
			 " but %qE has not yet been bound",
			 m_callee_fndecl, m_arg);
	      return true;
	    }
	  if (m_actual_state == m_sm.m_connected_stream_socket)
	    {
	      pp_printf (&pp,
			 "%qE expects a bound stream socket file descriptor"
			 " but %qE is connected",
			 m_callee_fndecl, m_arg);
	      return true;
	    }
	}
	break;
      case EXPECTED_PHASE_CAN_ACCEPT:
	{
	  if (m_actual_state == m_sm.m_new_stream_socket
	      || m_actual_state == m_sm.m_new_unknown_socket)
	    {
	      pp_printf (&pp,
			 "%qE expects a listening stream socket file descriptor"
			 " but %qE has not yet been bound",
			 m_callee_fndecl, m_arg);
	      return true;
	    }
	  if (m_actual_state == m_sm.m_bound_stream_socket
	      || m_actual_state == m_sm.m_bound_unknown_socket)
	    {
	      pp_printf (&pp,
			 "%qE expects a listening stream socket file descriptor"
			 " whereas %qE is bound but not yet listening",
			 m_callee_fndecl, m_arg);
	      return true;
	    }
	  if (m_actual_state == m_sm.m_connected_stream_socket)
	    {
	      pp_printf (&pp,
			 "%qE expects a listening stream socket file descriptor"
			 " but %qE is connected",
			 m_callee_fndecl, m_arg);
	      return true;
	    }
	}
	break;
      case EXPECTED_PHASE_CAN_CONNECT:
	{
	  if (m_actual_state == m_sm.m_bound_datagram_socket
	      || m_actual_state == m_sm.m_bound_stream_socket
	      || m_actual_state == m_sm.m_bound_unknown_socket)
	    {
	      pp_printf (&pp,
			 "%qE expects a new socket file descriptor"
			 " but %qE is bound",
			 m_callee_fndecl, m_arg);
	      return true;
	    }
	  else
	    {
	      pp_printf (&pp,
			 "%qE expects a new socket file descriptor",
			 m_callee_fndecl);
	      return true;
	    }
	}
	break;
      }
    gcc_unreachable ();
  }

private:
  state_machine::state_t m_actual_state;
  enum expected_phase m_expected_phase;
};

/* Enum for use by -Wanalyzer-fd-type-mismatch.  */

enum expected_type
{
 EXPECTED_TYPE_SOCKET,
 EXPECTED_TYPE_STREAM_SOCKET
};

/* Concrete pending_diagnostic subclass for -Wanalyzer-fd-type-mismatch.  */

class fd_type_mismatch : public fd_param_diagnostic
{
public:
  fd_type_mismatch (const fd_state_machine &sm, tree arg,
		    const tree callee_fndecl,
		    state_machine::state_t actual_state,
		    enum expected_type expected_type)
  : fd_param_diagnostic (sm, arg, callee_fndecl),
    m_actual_state (actual_state),
    m_expected_type (expected_type)
  {
  }

  const char *
  get_kind () const final override
  {
    return "fd_type_mismatch";
  }

  bool
  subclass_equal_p (const pending_diagnostic &base_other) const final override
  {
    const fd_type_mismatch &sub_other = (const fd_type_mismatch &)base_other;
    if (!fd_param_diagnostic ::subclass_equal_p (sub_other))
      return false;
    return (m_actual_state == sub_other.m_actual_state
	    && m_expected_type == sub_other.m_expected_type);
  }

  int
  get_controlling_option () const final override
  {
    return OPT_Wanalyzer_fd_type_mismatch;
  }

  bool
  emit (diagnostic_emission_context &ctxt) final override
  {
    switch (m_expected_type)
      {
      default:
	gcc_unreachable ();
      case EXPECTED_TYPE_SOCKET:
	return ctxt.warn ("%qE on non-socket file descriptor %qE",
			  m_callee_fndecl, m_arg);
      case EXPECTED_TYPE_STREAM_SOCKET:
	if (m_sm.is_datagram_socket_fd_p (m_actual_state))
	  return ctxt.warn ("%qE on datagram socket file descriptor %qE",
			    m_callee_fndecl, m_arg);
	else
	  return ctxt.warn ("%qE on non-stream-socket file descriptor %qE",
			    m_callee_fndecl, m_arg);
      }
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    switch (m_expected_type)
      {
      default:
	break;
	gcc_unreachable ();
      case EXPECTED_TYPE_SOCKET:
      case EXPECTED_TYPE_STREAM_SOCKET:
	if (!m_sm.is_socket_fd_p (m_actual_state))
	  {
	    pp_printf (&pp,
		       "%qE expects a socket file descriptor"
		       " but %qE is not a socket",
		       m_callee_fndecl, m_arg);
	    return true;
	  }
      }
    gcc_assert (m_expected_type == EXPECTED_TYPE_STREAM_SOCKET);
    gcc_assert (m_sm.is_datagram_socket_fd_p (m_actual_state));
    pp_printf (&pp,
	       "%qE expects a stream socket file descriptor"
	       " but %qE is a datagram socket",
	       m_callee_fndecl, m_arg);
    return true;
  }

private:
  state_machine::state_t m_actual_state;
  enum expected_type m_expected_type;
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
      m_new_datagram_socket (add_state ("fd-new-datagram-socket")),
      m_new_stream_socket (add_state ("fd-new-stream-socket")),
      m_new_unknown_socket (add_state ("fd-new-unknown-socket")),
      m_bound_datagram_socket (add_state ("fd-bound-datagram-socket")),
      m_bound_stream_socket (add_state ("fd-bound-stream-socket")),
      m_bound_unknown_socket (add_state ("fd-bound-unknown-socket")),
      m_listening_stream_socket (add_state ("fd-listening-stream-socket")),
      m_connected_stream_socket (add_state ("fd-connected-stream-socket")),
      m_stop (add_state ("fd-stop")),
      m_O_ACCMODE (get_stashed_constant_by_name ("O_ACCMODE")),
      m_O_RDONLY (get_stashed_constant_by_name ("O_RDONLY")),
      m_O_WRONLY (get_stashed_constant_by_name ("O_WRONLY")),
      m_SOCK_STREAM (get_stashed_constant_by_name ("SOCK_STREAM")),
      m_SOCK_DGRAM (get_stashed_constant_by_name ("SOCK_DGRAM"))
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

bool
fd_state_machine::is_socket_fd_p (state_t s) const
{
  return (s == m_new_datagram_socket
	  || s == m_new_stream_socket
	  || s == m_new_unknown_socket
	  || s == m_bound_datagram_socket
	  || s == m_bound_stream_socket
	  || s == m_bound_unknown_socket
	  || s == m_listening_stream_socket
	  || s == m_connected_stream_socket);
}

bool
fd_state_machine::is_datagram_socket_fd_p (state_t s) const
{
  return (s == m_new_datagram_socket
	  || s == m_new_unknown_socket
	  || s == m_bound_datagram_socket
	  || s == m_bound_unknown_socket);
}

bool
fd_state_machine::is_stream_socket_fd_p (state_t s) const
{
  return (s == m_new_stream_socket
	  || s == m_new_unknown_socket
	  || s == m_bound_stream_socket
	  || s == m_bound_unknown_socket
	  || s == m_listening_stream_socket
	  || s == m_connected_stream_socket);
}

enum access_mode
fd_state_machine::get_access_mode_from_flag (int flag) const
{
  if (m_O_ACCMODE && TREE_CODE (m_O_ACCMODE) == INTEGER_CST)
    {
      const unsigned HOST_WIDE_INT mask_val = TREE_INT_CST_LOW (m_O_ACCMODE);
      const unsigned HOST_WIDE_INT masked_flag = flag & mask_val;

      if (m_O_RDONLY && TREE_CODE (m_O_RDONLY) == INTEGER_CST)
	if (masked_flag == TREE_INT_CST_LOW (m_O_RDONLY))
	  return READ_ONLY;

      if (m_O_WRONLY && TREE_CODE (m_O_WRONLY) == INTEGER_CST)
	if (masked_flag == TREE_INT_CST_LOW (m_O_WRONLY))
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

void
fd_state_machine::mark_as_valid_fd (region_model *model,
				    sm_state_map *smap,
				    const svalue *fd_sval,
				    const extrinsic_state &ext_state) const
{
  smap->set_state (model, fd_sval, m_valid_read_write, NULL, ext_state);
}

bool
fd_state_machine::on_stmt (sm_context &sm_ctxt, const supernode *node,
			   const gimple *stmt) const
{
  if (const gcall *call = dyn_cast<const gcall *> (stmt))
    if (tree callee_fndecl = sm_ctxt.get_fndecl_for_call (call))
      {
	if (is_named_call_p (callee_fndecl, "open", call, 2))
	  {
	    on_open (sm_ctxt, node, stmt, call);
	    return true;
	  } //  "open"

	if (is_named_call_p (callee_fndecl, "creat", call, 2))
	  {
	    on_creat (sm_ctxt, node, stmt, call);
	    return true;
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
    sm_context &sm_ctxt, const supernode *node, const gimple *stmt,
    const gcall *call, const tree callee_fndecl, const char *attr_name,
    access_directions fd_attr_access_dir) const
{
  /* Handle interesting fd attributes of the callee_fndecl,
     or prioritize those of the builtin that callee_fndecl is
     expected to be.
     Might want this to be controlled by a flag.  */
  tree fndecl = callee_fndecl;
  /* If call is recognized as a builtin known_function,
     use that builtin's function_decl.  */
  if (const region_model *old_model = sm_ctxt.get_old_region_model ())
    if (const builtin_known_function *builtin_kf
	 = old_model->get_builtin_kf (call))
      fndecl = builtin_kf->builtin_decl ();

  tree attrs = TYPE_ATTRIBUTES (TREE_TYPE (fndecl));
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
      tree diag_arg = sm_ctxt.get_diagnostic_tree (arg);
      state_t state = sm_ctxt.get_state (stmt, arg);
      bool bit_set = bitmap_bit_p (argmap, arg_idx);
      if (TREE_CODE (TREE_TYPE (arg)) != INTEGER_TYPE)
	continue;
      if (bit_set) // Check if arg_idx is marked by any of the file descriptor
		   // attributes
	{

	  /* Do use the fndecl that caused the warning so that the
	     misused attributes are printed and the user not confused.  */
	  if (is_closed_fd_p (state))
	    {

	      sm_ctxt.warn (node, stmt, arg,
			    make_unique<fd_use_after_close>
			    (*this, diag_arg,
			     fndecl, attr_name,
			     arg_idx));
	      continue;
	    }

	  if (!(is_valid_fd_p (state) || (state == m_stop)))
	    {
	      if (!is_constant_fd_p (state))
		{
		  sm_ctxt.warn (node, stmt, arg,
				make_unique<fd_use_without_check>
				(*this, diag_arg,
				 fndecl, attr_name,
				 arg_idx));
		  continue;
		}
	    }

	  switch (fd_attr_access_dir)
	    {
	    case DIRS_READ_WRITE:
	      break;
	    case DIRS_READ:

	      if (is_writeonly_fd_p (state))
		{
		  sm_ctxt.warn (
		      node, stmt, arg,
		      make_unique<fd_access_mode_mismatch> (*this, diag_arg,
							    DIRS_WRITE,
							    fndecl,
							    attr_name,
							    arg_idx));
		}

	      break;
	    case DIRS_WRITE:

	      if (is_readonly_fd_p (state))
		{
		  sm_ctxt.warn (
		      node, stmt, arg,
		      make_unique<fd_access_mode_mismatch> (*this, diag_arg,
							    DIRS_READ,
							    fndecl,
							    attr_name,
							    arg_idx));
		}

	      break;
	    }
	}
    }
}


void
fd_state_machine::on_open (sm_context &sm_ctxt, const supernode *node,
			   const gimple *stmt, const gcall *call) const
{
  tree lhs = gimple_call_lhs (call);
  if (lhs)
    {
      tree arg = gimple_call_arg (call, 1);
      enum access_mode mode = READ_WRITE;
      if (TREE_CODE (arg) == INTEGER_CST)
	{
	  int flag = TREE_INT_CST_LOW (arg);
	  mode = get_access_mode_from_flag (flag);
	}
      switch (mode)
	{
	case READ_ONLY:
	  sm_ctxt.on_transition (node, stmt, lhs, m_start,
				 m_unchecked_read_only);
	  break;
	case WRITE_ONLY:
	  sm_ctxt.on_transition (node, stmt, lhs, m_start,
				 m_unchecked_write_only);
	  break;
	default:
	  sm_ctxt.on_transition (node, stmt, lhs, m_start,
				 m_unchecked_read_write);
	}
    }
  else
    {
      sm_ctxt.warn (node, stmt, NULL_TREE,
		    make_unique<fd_leak> (*this, NULL_TREE));
    }
}

void
fd_state_machine::on_creat (sm_context &sm_ctxt, const supernode *node,
			    const gimple *stmt, const gcall *call) const
{
  tree lhs = gimple_call_lhs (call);
  if (lhs)
    sm_ctxt.on_transition (node, stmt, lhs, m_start, m_unchecked_write_only);
  else
    sm_ctxt.warn (node, stmt, NULL_TREE,
		  make_unique<fd_leak> (*this, NULL_TREE));
}

void
fd_state_machine::check_for_dup (sm_context &sm_ctxt, const supernode *node,
				 const gimple *stmt, const gcall *call,
				 const tree callee_fndecl, enum dup kind) const
{
  tree lhs = gimple_call_lhs (call);
  tree arg_1 = gimple_call_arg (call, 0);
  state_t state_arg_1 = sm_ctxt.get_state (stmt, arg_1);
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
	    sm_ctxt.set_next_state (stmt, lhs, m_unchecked_read_write);
	  else
	    sm_ctxt.set_next_state (stmt, lhs,
				    valid_to_unchecked_state (state_arg_1));
	}
      break;

    case DUP_2:
    case DUP_3:
      tree arg_2 = gimple_call_arg (call, 1);
      state_t state_arg_2 = sm_ctxt.get_state (stmt, arg_2);
      tree diag_arg_2 = sm_ctxt.get_diagnostic_tree (arg_2);
      if (state_arg_2 == m_stop)
	return;
      /* Check if -1 was passed as second argument to dup2.  */
      if (!(is_constant_fd_p (state_arg_2) || is_valid_fd_p (state_arg_2)
	    || state_arg_2 == m_start))
	{
	  sm_ctxt.warn (
	      node, stmt, arg_2,
	      make_unique<fd_use_without_check> (*this, diag_arg_2,
						 callee_fndecl));
	  return;
	}
      /* dup2 returns value of its second argument on success.But, the
      access mode of the returned file descriptor depends on the duplicated
      file descriptor i.e the first argument.  */
      if (lhs)
	{
	  if (is_constant_fd_p (state_arg_1) || state_arg_1 == m_start)
	    sm_ctxt.set_next_state (stmt, lhs, m_unchecked_read_write);
	  else
	    sm_ctxt.set_next_state (stmt, lhs,
				    valid_to_unchecked_state (state_arg_1));
	}

      break;
    }
}

void
fd_state_machine::on_close (sm_context &sm_ctxt, const supernode *node,
			    const gimple *stmt, const gcall *call) const
{
  tree arg = gimple_call_arg (call, 0);
  state_t state = sm_ctxt.get_state (stmt, arg);
  tree diag_arg = sm_ctxt.get_diagnostic_tree (arg);

  sm_ctxt.on_transition (node, stmt, arg, m_start, m_closed);
  sm_ctxt.on_transition (node, stmt, arg, m_unchecked_read_write, m_closed);
  sm_ctxt.on_transition (node, stmt, arg, m_unchecked_read_only, m_closed);
  sm_ctxt.on_transition (node, stmt, arg, m_unchecked_write_only, m_closed);
  sm_ctxt.on_transition (node, stmt, arg, m_valid_read_write, m_closed);
  sm_ctxt.on_transition (node, stmt, arg, m_valid_read_only, m_closed);
  sm_ctxt.on_transition (node, stmt, arg, m_valid_write_only, m_closed);
  sm_ctxt.on_transition (node, stmt, arg, m_constant_fd, m_closed);
  sm_ctxt.on_transition (node, stmt, arg, m_new_datagram_socket, m_closed);
  sm_ctxt.on_transition (node, stmt, arg, m_new_stream_socket, m_closed);
  sm_ctxt.on_transition (node, stmt, arg, m_new_unknown_socket, m_closed);
  sm_ctxt.on_transition (node, stmt, arg, m_bound_datagram_socket, m_closed);
  sm_ctxt.on_transition (node, stmt, arg, m_bound_stream_socket, m_closed);
  sm_ctxt.on_transition (node, stmt, arg, m_bound_unknown_socket, m_closed);
  sm_ctxt.on_transition (node, stmt, arg, m_listening_stream_socket, m_closed);
  sm_ctxt.on_transition (node, stmt, arg, m_connected_stream_socket, m_closed);

  if (is_closed_fd_p (state))
    {
      sm_ctxt.warn (node, stmt, arg,
		    make_unique<fd_double_close> (*this, diag_arg));
      sm_ctxt.set_next_state (stmt, arg, m_stop);
    }
}
void
fd_state_machine::on_read (sm_context &sm_ctxt, const supernode *node,
			   const gimple *stmt, const gcall *call,
			   const tree callee_fndecl) const
{
  check_for_open_fd (sm_ctxt, node, stmt, call, callee_fndecl, DIRS_READ);
}
void
fd_state_machine::on_write (sm_context &sm_ctxt, const supernode *node,
			    const gimple *stmt, const gcall *call,
			    const tree callee_fndecl) const
{
  check_for_open_fd (sm_ctxt, node, stmt, call, callee_fndecl, DIRS_WRITE);
}

void
fd_state_machine::check_for_open_fd (
    sm_context &sm_ctxt, const supernode *node, const gimple *stmt,
    const gcall *call, const tree callee_fndecl,
    enum access_directions callee_fndecl_dir) const
{
  tree arg = gimple_call_arg (call, 0);
  tree diag_arg = sm_ctxt.get_diagnostic_tree (arg);
  state_t state = sm_ctxt.get_state (stmt, arg);

  if (is_closed_fd_p (state))
    {
      sm_ctxt.warn (node, stmt, arg,
		    make_unique<fd_use_after_close> (*this, diag_arg,
						     callee_fndecl));
    }

  else
    {
      if (state == m_new_stream_socket
	  || state == m_bound_stream_socket
	  || state == m_listening_stream_socket)
	/* Complain about fncall on socket in wrong phase.  */
	sm_ctxt.warn
	  (node, stmt, arg,
	   make_unique<fd_phase_mismatch> (*this, diag_arg,
					   callee_fndecl,
					   state,
					   EXPECTED_PHASE_CAN_TRANSFER));
      else if (!(is_valid_fd_p (state)
		 || state == m_new_datagram_socket
		 || state == m_bound_unknown_socket
		 || state == m_connected_stream_socket
		 || state == m_start
		 || state == m_stop))
	{
	  if (!is_constant_fd_p (state))
	    sm_ctxt.warn (
		node, stmt, arg,
		make_unique<fd_use_without_check> (*this, diag_arg,
						   callee_fndecl));
	}
      switch (callee_fndecl_dir)
	{
	case DIRS_READ_WRITE:
	  break;
	case DIRS_READ:
	  if (is_writeonly_fd_p (state))
	    {
	      tree diag_arg = sm_ctxt.get_diagnostic_tree (arg);
	      sm_ctxt.warn (node, stmt, arg,
			    make_unique<fd_access_mode_mismatch> (
			      *this, diag_arg, DIRS_WRITE, callee_fndecl));
	    }

	  break;
	case DIRS_WRITE:

	  if (is_readonly_fd_p (state))
	    {
	      tree diag_arg = sm_ctxt.get_diagnostic_tree (arg);
	      sm_ctxt.warn (node, stmt, arg,
			     make_unique<fd_access_mode_mismatch> (
				 *this, diag_arg, DIRS_READ, callee_fndecl));
	    }
	  break;
	}
    }
}

static bool
add_constraint_ge_zero (region_model *model,
			const svalue *fd_sval,
			region_model_context *ctxt)
{
  const svalue *zero
    = model->get_manager ()->get_or_create_int_cst (integer_type_node, 0);
  return model->add_constraint (fd_sval, GE_EXPR, zero, ctxt);
}

/* Get the state for a new socket type based on SOCKET_TYPE_SVAL,
   a SOCK_* value.  */

state_machine::state_t
fd_state_machine::
get_state_for_socket_type (const svalue *socket_type_sval) const
{
  if (tree socket_type_cst = socket_type_sval->maybe_get_constant ())
    {
      /* Attempt to use SOCK_* constants stashed from the frontend.  */
      if (tree_int_cst_equal (socket_type_cst, m_SOCK_STREAM))
	return m_new_stream_socket;
      if (tree_int_cst_equal (socket_type_cst, m_SOCK_DGRAM))
	return m_new_datagram_socket;
    }

  /* Unrecognized constant, or a symbolic "type" value.  */
  return m_new_unknown_socket;
}

/* Update the model and fd state for an outcome of a call to "socket",
   where SUCCESSFUL indicate which of the two outcomes.
   Return true if the outcome is feasible, or false to reject it.  */

bool
fd_state_machine::on_socket (const call_details &cd,
			     bool successful,
			     sm_context &sm_ctxt,
			     const extrinsic_state &ext_state) const
{
  const gcall *stmt = cd.get_call_stmt ();
  engine *eng = ext_state.get_engine ();
  const supergraph *sg = eng->get_supergraph ();
  const supernode *node = sg->get_supernode_for_stmt (stmt);
  region_model *model = cd.get_model ();

  if (successful)
    {
      if (gimple_call_lhs (stmt))
	{
	  conjured_purge p (model, cd.get_ctxt ());
	  region_model_manager *mgr = model->get_manager ();
	  const svalue *new_fd
	    = mgr->get_or_create_conjured_svalue (integer_type_node,
						  stmt,
						  cd.get_lhs_region (),
						  p);
	  if (!add_constraint_ge_zero (model, new_fd, cd.get_ctxt ()))
	    return false;

	  const svalue *socket_type_sval = cd.get_arg_svalue (1);
	  state_machine::state_t new_state
	    = get_state_for_socket_type (socket_type_sval);
	  sm_ctxt.on_transition (node, stmt, new_fd, m_start, new_state);
	  model->set_value (cd.get_lhs_region (), new_fd, cd.get_ctxt ());
	}
      else
	sm_ctxt.warn (node, stmt, NULL_TREE,
		      make_unique<fd_leak> (*this, NULL_TREE));
    }
  else
    {
      /* Return -1; set errno.  */
      model->update_for_int_cst_return (cd, -1, true);
      model->set_errno (cd);
    }

  return true;
}

/* Check that FD_SVAL is usable by socket APIs.
   Complain if it has been closed, if it is a non-socket,
   or is invalid.
   If COMPLAINED is non-NULL and a problem is found,
   write *COMPLAINED = true.

   If SUCCESSFUL is true, attempt to add the constraint that FD_SVAL >= 0.
   Return true if this outcome is feasible.  */

bool
fd_state_machine::check_for_socket_fd (const call_details &cd,
				       bool successful,
				       sm_context &sm_ctxt,
				       const svalue *fd_sval,
				       const supernode *node,
				       state_t old_state,
				       bool *complained) const
{
  const gcall *stmt = cd.get_call_stmt ();

  if (is_closed_fd_p (old_state))
    {
      tree diag_arg = sm_ctxt.get_diagnostic_tree (fd_sval);
      sm_ctxt.warn
	(node, stmt, fd_sval,
	 make_unique<fd_use_after_close> (*this, diag_arg,
					  cd.get_fndecl_for_call ()));
      if (complained)
	*complained = true;
      if (successful)
	return false;
    }
  else if (is_unchecked_fd_p (old_state) || is_valid_fd_p (old_state))
    {
      /* Complain about non-socket.  */
      tree diag_arg = sm_ctxt.get_diagnostic_tree (fd_sval);
      sm_ctxt.warn
	(node, stmt, fd_sval,
	 make_unique<fd_type_mismatch> (*this, diag_arg,
					cd.get_fndecl_for_call (),
					old_state,
					EXPECTED_TYPE_SOCKET));
      if (complained)
	*complained = true;
      if (successful)
	return false;
    }
  else if (old_state == m_invalid)
    {
      tree diag_arg = sm_ctxt.get_diagnostic_tree (fd_sval);
      sm_ctxt.warn
	(node, stmt, fd_sval,
	 make_unique<fd_use_without_check> (*this, diag_arg,
					    cd.get_fndecl_for_call ()));
      if (complained)
	*complained = true;
      if (successful)
	return false;
    }

  if (successful)
    if (!add_constraint_ge_zero (cd.get_model (), fd_sval, cd.get_ctxt ()))
      return false;

  return true;
}

/* For use by "bind" and "connect".
   As per fd_state_machine::check_for_socket_fd above,
   but also complain if we don't have a new socket, and check that
   we can read up to the size bytes from the address.  */

bool
fd_state_machine::check_for_new_socket_fd (const call_details &cd,
					   bool successful,
					   sm_context &sm_ctxt,
					   const svalue *fd_sval,
					   const supernode *node,
					   state_t old_state,
					   enum expected_phase expected_phase)
  const
{
  bool complained = false;

  /* Check address and len.  */
  const svalue *address_sval = cd.get_arg_svalue (1);
  const svalue *len_sval = cd.get_arg_svalue (2);

  /* Check that we can read the given number of bytes from the
     address.  */
  region_model *model = cd.get_model ();
  const region *address_reg
    = model->deref_rvalue (address_sval, cd.get_arg_tree (1),
			   cd.get_ctxt ());
  const region *sized_address_reg
    = model->get_manager ()->get_sized_region (address_reg,
					       NULL_TREE,
					       len_sval);
  model->get_store_value (sized_address_reg, cd.get_ctxt ());

  if (!check_for_socket_fd (cd, successful, sm_ctxt,
			    fd_sval, node, old_state, &complained))
    return false;
  else if (!complained
	   && !(old_state == m_new_stream_socket
		|| old_state == m_new_datagram_socket
		|| old_state == m_new_unknown_socket
		|| old_state == m_start
		|| old_state == m_stop
		|| old_state == m_constant_fd))
    {
      /* Complain about "bind" or "connect" in wrong phase.  */
      tree diag_arg = sm_ctxt.get_diagnostic_tree (fd_sval);
      sm_ctxt.warn
	(node, cd.get_call_stmt (), fd_sval,
	 make_unique<fd_phase_mismatch> (*this, diag_arg,
					 cd.get_fndecl_for_call (),
					 old_state,
					 expected_phase));
      if (successful)
	return false;
    }
  else if (!successful)
    {
      /* If we were in the start state, assume we had a new socket.  */
      if (old_state == m_start)
	sm_ctxt.set_next_state (cd.get_call_stmt (), fd_sval,
				m_new_unknown_socket);
    }

  /* Passing NULL as the address will lead to failure.  */
  if (successful)
    if (address_sval->all_zeroes_p ())
      return false;

  return true;
}

/* Update the model and fd state for an outcome of a call to "bind",
   where SUCCESSFUL indicate which of the two outcomes.
   Return true if the outcome is feasible, or false to reject it.  */

bool
fd_state_machine::on_bind (const call_details &cd,
			   bool successful,
			   sm_context &sm_ctxt,
			   const extrinsic_state &ext_state) const
{
  const gcall *stmt = cd.get_call_stmt ();
  engine *eng = ext_state.get_engine ();
  const supergraph *sg = eng->get_supergraph ();
  const supernode *node = sg->get_supernode_for_stmt (stmt);
  const svalue *fd_sval = cd.get_arg_svalue (0);
  region_model *model = cd.get_model ();
  state_t old_state = sm_ctxt.get_state (stmt, fd_sval);

  if (!check_for_new_socket_fd (cd, successful, sm_ctxt,
				fd_sval, node, old_state,
				EXPECTED_PHASE_CAN_BIND))
    return false;

  if (successful)
    {
      state_t next_state = NULL;
      if (old_state == m_new_stream_socket)
	next_state = m_bound_stream_socket;
      else if (old_state == m_new_datagram_socket)
	next_state = m_bound_datagram_socket;
      else if (old_state == m_new_unknown_socket)
	next_state = m_bound_unknown_socket;
      else if (old_state == m_start
	       || old_state == m_constant_fd)
	next_state = m_bound_unknown_socket;
      else if (old_state == m_stop)
	next_state = m_stop;
      else
	gcc_unreachable ();
      sm_ctxt.set_next_state (cd.get_call_stmt (), fd_sval, next_state);
      model->update_for_zero_return (cd, true);
    }
  else
    {
      /* Return -1; set errno.  */
      model->update_for_int_cst_return (cd, -1, true);
      model->set_errno (cd);
    }

  return true;
}

/* Update the model and fd state for an outcome of a call to "listen",
   where SUCCESSFUL indicate which of the two outcomes.
   Return true if the outcome is feasible, or false to reject it.  */

bool
fd_state_machine::on_listen (const call_details &cd,
			     bool successful,
			     sm_context &sm_ctxt,
			     const extrinsic_state &ext_state) const
{
  const gcall *stmt = cd.get_call_stmt ();
  engine *eng = ext_state.get_engine ();
  const supergraph *sg = eng->get_supergraph ();
  const supernode *node = sg->get_supernode_for_stmt (cd.get_call_stmt ());
  const svalue *fd_sval = cd.get_arg_svalue (0);
  region_model *model = cd.get_model ();
  state_t old_state = sm_ctxt.get_state (stmt, fd_sval);

  /* We expect a stream socket that's had "bind" called on it.  */
  if (!check_for_socket_fd (cd, successful, sm_ctxt, fd_sval, node, old_state))
    return false;
  if (!(old_state == m_start
	|| old_state == m_constant_fd
	|| old_state == m_stop
	|| old_state == m_invalid
	|| old_state == m_bound_stream_socket
	|| old_state == m_bound_unknown_socket
	/* Assume it's OK to call "listen" more than once.  */
	|| old_state == m_listening_stream_socket))
    {
      /* Complain about fncall on wrong type or in wrong phase.  */
      tree diag_arg = sm_ctxt.get_diagnostic_tree (fd_sval);
      if (is_stream_socket_fd_p (old_state))
	sm_ctxt.warn
	  (node, stmt, fd_sval,
	   make_unique<fd_phase_mismatch> (*this, diag_arg,
					   cd.get_fndecl_for_call (),
					   old_state,
					   EXPECTED_PHASE_CAN_LISTEN));
      else
	sm_ctxt.warn
	  (node, stmt, fd_sval,
	   make_unique<fd_type_mismatch> (*this, diag_arg,
					  cd.get_fndecl_for_call (),
					  old_state,
					  EXPECTED_TYPE_STREAM_SOCKET));
      if (successful)
	return false;
    }

  if (successful)
    {
      model->update_for_zero_return (cd, true);
      sm_ctxt.set_next_state (cd.get_call_stmt (), fd_sval,
			      m_listening_stream_socket);
    }
  else
    {
      /* Return -1; set errno.  */
      model->update_for_int_cst_return (cd, -1, true);
      model->set_errno (cd);
      if (old_state == m_start)
	sm_ctxt.set_next_state (cd.get_call_stmt (), fd_sval,
				m_bound_stream_socket);
    }

  return true;
}

/* Update the model and fd state for an outcome of a call to "accept",
   where SUCCESSFUL indicate which of the two outcomes.
   Return true if the outcome is feasible, or false to reject it.  */

bool
fd_state_machine::on_accept (const call_details &cd,
			     bool successful,
			     sm_context &sm_ctxt,
			     const extrinsic_state &ext_state) const
{
  const gcall *stmt = cd.get_call_stmt ();
  engine *eng = ext_state.get_engine ();
  const supergraph *sg = eng->get_supergraph ();
  const supernode *node = sg->get_supernode_for_stmt (stmt);
  const svalue *fd_sval = cd.get_arg_svalue (0);
  const svalue *address_sval = cd.get_arg_svalue (1);
  const svalue *len_ptr_sval = cd.get_arg_svalue (2);
  region_model *model = cd.get_model ();
  state_t old_state = sm_ctxt.get_state (stmt, fd_sval);

  if (!address_sval->all_zeroes_p ())
    {
      region_model_manager *mgr = model->get_manager ();

      /* We might have a union of various pointer types, rather than a
	 pointer type; cast to (void *) before dereferencing.  */
      address_sval = mgr->get_or_create_cast (ptr_type_node, address_sval);

      const region *address_reg
	= model->deref_rvalue (address_sval, cd.get_arg_tree (1),
			       cd.get_ctxt ());
      const region *len_reg
	= model->deref_rvalue (len_ptr_sval, cd.get_arg_tree (2),
			       cd.get_ctxt ());
      const svalue *old_len_sval
	= model->get_store_value (len_reg, cd.get_ctxt ());
      tree len_ptr = cd.get_arg_tree (2);
      tree star_len_ptr = build2 (MEM_REF, TREE_TYPE (TREE_TYPE (len_ptr)),
				  len_ptr,
				  build_int_cst (TREE_TYPE (len_ptr), 0));
      old_len_sval = model->check_for_poison (old_len_sval,
					      star_len_ptr,
					      len_reg,
					      cd.get_ctxt ());
      if (successful)
	{
	  conjured_purge p (model, cd.get_ctxt ());
	  const region *old_sized_address_reg
	    = mgr->get_sized_region (address_reg,
				     NULL_TREE,
				     old_len_sval);
	  const svalue *new_addr_sval
	    = mgr->get_or_create_conjured_svalue (NULL_TREE,
						  stmt,
						  old_sized_address_reg,
						  p);
	  model->set_value (old_sized_address_reg, new_addr_sval,
			    cd.get_ctxt ());
	  const svalue *new_addr_len
	    = mgr->get_or_create_conjured_svalue (NULL_TREE,
						  stmt,
						  len_reg,
						  p);
	  model->set_value (len_reg, new_addr_len, cd.get_ctxt ());
	}
    }

  /* We expect a stream socket in the "listening" state.  */
  if (!check_for_socket_fd (cd, successful, sm_ctxt, fd_sval, node, old_state))
    return false;

  if (old_state == m_start || old_state == m_constant_fd)
    /* If we were in the start state (or a constant), assume we had the
       expected state.  */
    sm_ctxt.set_next_state (cd.get_call_stmt (), fd_sval,
			    m_listening_stream_socket);
  else if (old_state == m_stop)
    {
      /* No further complaints.  */
    }
  else if (old_state != m_listening_stream_socket)
    {
      /* Complain about fncall on wrong type or in wrong phase.  */
      tree diag_arg = sm_ctxt.get_diagnostic_tree (fd_sval);
      if (is_stream_socket_fd_p (old_state))
	sm_ctxt.warn
	  (node, stmt, fd_sval,
	   make_unique<fd_phase_mismatch> (*this, diag_arg,
					   cd.get_fndecl_for_call (),
					   old_state,
					   EXPECTED_PHASE_CAN_ACCEPT));
      else
	sm_ctxt.warn
	  (node, stmt, fd_sval,
	   make_unique<fd_type_mismatch> (*this, diag_arg,
					  cd.get_fndecl_for_call (),
					  old_state,
					  EXPECTED_TYPE_STREAM_SOCKET));
      if (successful)
	return false;
    }

  if (successful)
    {
      /* Return new conjured FD in "connected" state.  */
      if (gimple_call_lhs (stmt))
	{
	  conjured_purge p (model, cd.get_ctxt ());
	  region_model_manager *mgr = model->get_manager ();
	  const svalue *new_fd
	    = mgr->get_or_create_conjured_svalue (integer_type_node,
						  stmt,
						  cd.get_lhs_region (),
						  p);
	  if (!add_constraint_ge_zero (model, new_fd, cd.get_ctxt ()))
	    return false;
	  sm_ctxt.on_transition (node, stmt, new_fd,
				 m_start, m_connected_stream_socket);
	  model->set_value (cd.get_lhs_region (), new_fd, cd.get_ctxt ());
	}
      else
	sm_ctxt.warn (node, stmt, NULL_TREE,
		      make_unique<fd_leak> (*this, NULL_TREE));
    }
  else
    {
      /* Return -1; set errno.  */
      model->update_for_int_cst_return (cd, -1, true);
      model->set_errno (cd);
    }

  return true;
}

/* Update the model and fd state for an outcome of a call to "connect",
   where SUCCESSFUL indicate which of the two outcomes.
   Return true if the outcome is feasible, or false to reject it.  */

bool
fd_state_machine::on_connect (const call_details &cd,
			      bool successful,
			      sm_context &sm_ctxt,
			      const extrinsic_state &ext_state) const
{
  const gcall *stmt = cd.get_call_stmt ();
  engine *eng = ext_state.get_engine ();
  const supergraph *sg = eng->get_supergraph ();
  const supernode *node = sg->get_supernode_for_stmt (stmt);
  const svalue *fd_sval = cd.get_arg_svalue (0);
  region_model *model = cd.get_model ();
  state_t old_state = sm_ctxt.get_state (stmt, fd_sval);

  if (!check_for_new_socket_fd (cd, successful, sm_ctxt,
				fd_sval, node, old_state,
				EXPECTED_PHASE_CAN_CONNECT))
    return false;

  if (successful)
    {
      model->update_for_zero_return (cd, true);
      state_t next_state = NULL;
      if (old_state == m_new_stream_socket)
	next_state = m_connected_stream_socket;
      else if (old_state == m_new_datagram_socket)
	/* It's legal to call connect on a datagram socket, potentially
	   more than once.  We don't transition states for this.  */
	next_state = m_new_datagram_socket;
      else if (old_state == m_new_unknown_socket)
	next_state = m_stop;
      else if (old_state == m_start
	       || old_state == m_constant_fd)
	next_state = m_stop;
      else if (old_state == m_stop)
	next_state = m_stop;
      else
	gcc_unreachable ();
      sm_ctxt.set_next_state (cd.get_call_stmt (), fd_sval, next_state);
    }
  else
    {
      /* Return -1; set errno.  */
      model->update_for_int_cst_return (cd, -1, true);
      model->set_errno (cd);
      /* TODO: perhaps transition to a failed state, since the
	 portable way to handle a failed "connect" is to close
	 the socket and try again with a new socket.  */
    }

  return true;
}

void
fd_state_machine::on_condition (sm_context &sm_ctxt, const supernode *node,
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
fd_state_machine::make_valid_transitions_on_condition (sm_context &sm_ctxt,
						       const supernode *node,
						       const gimple *stmt,
						       const svalue *lhs) const
{
  sm_ctxt.on_transition (node, stmt, lhs, m_unchecked_read_write,
			 m_valid_read_write);
  sm_ctxt.on_transition (node, stmt, lhs, m_unchecked_read_only,
			 m_valid_read_only);
  sm_ctxt.on_transition (node, stmt, lhs, m_unchecked_write_only,
			 m_valid_write_only);
}

void
fd_state_machine::make_invalid_transitions_on_condition (
    sm_context &sm_ctxt, const supernode *node, const gimple *stmt,
    const svalue *lhs) const
{
  sm_ctxt.on_transition (node, stmt, lhs, m_unchecked_read_write, m_invalid);
  sm_ctxt.on_transition (node, stmt, lhs, m_unchecked_read_only, m_invalid);
  sm_ctxt.on_transition (node, stmt, lhs, m_unchecked_write_only, m_invalid);
}

bool
fd_state_machine::can_purge_p (state_t s) const
{
  if (is_unchecked_fd_p (s)
      || is_valid_fd_p (s)
      || is_socket_fd_p (s))
    return false;
  else
    return true;
}

std::unique_ptr<pending_diagnostic>
fd_state_machine::on_leak (tree var) const
{
  return make_unique<fd_leak> (*this, var);
}
} // namespace

state_machine *
make_fd_state_machine (logger *logger)
{
  return new fd_state_machine (logger);
}

static bool
get_fd_state (region_model_context *ctxt,
	      sm_state_map **out_smap,
	      const fd_state_machine **out_sm,
	      unsigned *out_sm_idx,
	      std::unique_ptr<sm_context> *out_sm_context)
{
  if (!ctxt)
    return false;

  const state_machine *sm;
  if (!ctxt->get_fd_map (out_smap, &sm, out_sm_idx, out_sm_context))
    return false;

  gcc_assert (sm);

  *out_sm = (const fd_state_machine *)sm;
  return true;
}

/* Specialcase hook for handling pipe, for use by
   kf_pipe::success::update_model.  */

void
region_model::mark_as_valid_fd (const svalue *sval, region_model_context *ctxt)
{
  sm_state_map *smap;
  const fd_state_machine *fd_sm;
  if (!get_fd_state (ctxt, &smap, &fd_sm, NULL, NULL))
    return;
  const extrinsic_state *ext_state = ctxt->get_ext_state ();
  if (!ext_state)
    return;
  fd_sm->mark_as_valid_fd (this, smap, sval, *ext_state);
}

/* Handle calls to "socket".
   See e.g. https://man7.org/linux/man-pages/man3/socket.3p.html  */

class kf_socket : public known_function
{
public:
  class outcome_of_socket : public succeed_or_fail_call_info
  {
  public:
    outcome_of_socket (const call_details &cd, bool success)
    : succeed_or_fail_call_info (cd, success)
    {}

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      const call_details cd (get_call_details (model, ctxt));
      sm_state_map *smap;
      const fd_state_machine *fd_sm;
      std::unique_ptr<sm_context> sm_ctxt;
      if (!get_fd_state (ctxt, &smap, &fd_sm, NULL, &sm_ctxt))
	{
	  cd.set_any_lhs_with_defaults ();
	  return true;
	}
      const extrinsic_state *ext_state = ctxt->get_ext_state ();
      if (!ext_state)
	{
	  cd.set_any_lhs_with_defaults ();
	  return true;
	}

      return fd_sm->on_socket (cd, m_success, *(sm_ctxt.get ()), *ext_state);
    }
  };

  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == 3;
  }

  void impl_call_post (const call_details &cd) const final override
  {
    if (cd.get_ctxt ())
      {
	cd.get_ctxt ()->bifurcate (make_unique<outcome_of_socket> (cd, false));
	cd.get_ctxt ()->bifurcate (make_unique<outcome_of_socket> (cd, true));
	cd.get_ctxt ()->terminate_path ();
      }
  }
};

/* Handle calls to "bind".
   See e.g. https://man7.org/linux/man-pages/man3/bind.3p.html  */

class kf_bind : public known_function
{
public:
  class outcome_of_bind : public succeed_or_fail_call_info
  {
  public:
    outcome_of_bind (const call_details &cd, bool success)
    : succeed_or_fail_call_info (cd, success)
    {}

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      const call_details cd (get_call_details (model, ctxt));
      sm_state_map *smap;
      const fd_state_machine *fd_sm;
      std::unique_ptr<sm_context> sm_ctxt;
      if (!get_fd_state (ctxt, &smap, &fd_sm, NULL, &sm_ctxt))
	{
	  cd.set_any_lhs_with_defaults ();
	  return true;
	}
      const extrinsic_state *ext_state = ctxt->get_ext_state ();
      if (!ext_state)
	{
	  cd.set_any_lhs_with_defaults ();
	  return true;
	}
      return fd_sm->on_bind (cd, m_success, *sm_ctxt.get (), *ext_state);
    }
  };

  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 3 && cd.arg_is_pointer_p (1));
  }

  void impl_call_post (const call_details &cd) const final override
  {
    if (cd.get_ctxt ())
      {
	cd.get_ctxt ()->bifurcate (make_unique<outcome_of_bind> (cd, false));
	cd.get_ctxt ()->bifurcate (make_unique<outcome_of_bind> (cd, true));
	cd.get_ctxt ()->terminate_path ();
      }
  }
};

/* Handle calls to "listen".
   See e.g. https://man7.org/linux/man-pages/man3/listen.3p.html  */

class kf_listen : public known_function
{
  class outcome_of_listen : public succeed_or_fail_call_info
  {
  public:
    outcome_of_listen (const call_details &cd, bool success)
    : succeed_or_fail_call_info (cd, success)
    {}

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      const call_details cd (get_call_details (model, ctxt));
      sm_state_map *smap;
      const fd_state_machine *fd_sm;
      std::unique_ptr<sm_context> sm_ctxt;
      if (!get_fd_state (ctxt, &smap, &fd_sm, NULL, &sm_ctxt))
	{
	  cd.set_any_lhs_with_defaults ();
	  return true;
	}
      const extrinsic_state *ext_state = ctxt->get_ext_state ();
      if (!ext_state)
	{
	  cd.set_any_lhs_with_defaults ();
	  return true;
	}

      return fd_sm->on_listen (cd, m_success, *sm_ctxt.get (), *ext_state);
    }
  };

  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == 2;
  }

  void impl_call_post (const call_details &cd) const final override
  {
    if (cd.get_ctxt ())
      {
	cd.get_ctxt ()->bifurcate (make_unique<outcome_of_listen> (cd, false));
	cd.get_ctxt ()->bifurcate (make_unique<outcome_of_listen> (cd, true));
	cd.get_ctxt ()->terminate_path ();
      }
  }
};

/* Handle calls to "accept".
   See e.g. https://man7.org/linux/man-pages/man3/accept.3p.html  */

class kf_accept : public known_function
{
  class outcome_of_accept : public succeed_or_fail_call_info
  {
  public:
    outcome_of_accept (const call_details &cd, bool success)
    : succeed_or_fail_call_info (cd, success)
    {}

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      const call_details cd (get_call_details (model, ctxt));
      sm_state_map *smap;
      const fd_state_machine *fd_sm;
      std::unique_ptr<sm_context> sm_ctxt;
      if (!get_fd_state (ctxt, &smap, &fd_sm, NULL, &sm_ctxt))
	{
	  cd.set_any_lhs_with_defaults ();
	  return true;
	}
      const extrinsic_state *ext_state = ctxt->get_ext_state ();
      if (!ext_state)
	{
	  cd.set_any_lhs_with_defaults ();
	  return true;
	}

      return fd_sm->on_accept (cd, m_success, *sm_ctxt.get (), *ext_state);
    }
  };

  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 3
	    && cd.arg_is_pointer_p (1)
	    && cd.arg_is_pointer_p (2));
  }

  void impl_call_post (const call_details &cd) const final override
  {
    if (cd.get_ctxt ())
      {
	cd.get_ctxt ()->bifurcate (make_unique<outcome_of_accept> (cd, false));
	cd.get_ctxt ()->bifurcate (make_unique<outcome_of_accept> (cd, true));
	cd.get_ctxt ()->terminate_path ();
      }
  }
};

/* Handle calls to "connect".
   See e.g. https://man7.org/linux/man-pages/man3/connect.3p.html  */

class kf_connect : public known_function
{
public:
  class outcome_of_connect : public succeed_or_fail_call_info
  {
  public:
    outcome_of_connect (const call_details &cd, bool success)
    : succeed_or_fail_call_info (cd, success)
    {}

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      const call_details cd (get_call_details (model, ctxt));
      sm_state_map *smap;
      const fd_state_machine *fd_sm;
      std::unique_ptr<sm_context> sm_ctxt;
      if (!get_fd_state (ctxt, &smap, &fd_sm, NULL, &sm_ctxt))
	{
	  cd.set_any_lhs_with_defaults ();
	  return true;
	}
      const extrinsic_state *ext_state = ctxt->get_ext_state ();
      if (!ext_state)
	{
	  cd.set_any_lhs_with_defaults ();
	  return true;
	}

      return fd_sm->on_connect (cd, m_success, *sm_ctxt.get (), *ext_state);
    }
  };

  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 3
	    && cd.arg_is_pointer_p (1));
  }

  void impl_call_post (const call_details &cd) const final override
  {
    if (cd.get_ctxt ())
      {
	cd.get_ctxt ()->bifurcate (make_unique<outcome_of_connect> (cd, false));
	cd.get_ctxt ()->bifurcate (make_unique<outcome_of_connect> (cd, true));
	cd.get_ctxt ()->terminate_path ();
      }
  }
};

/* Handler for "isatty"".
   See e.g. https://man7.org/linux/man-pages/man3/isatty.3.html  */

class kf_isatty : public known_function
{
  class outcome_of_isatty : public succeed_or_fail_call_info
  {
  public:
    outcome_of_isatty (const call_details &cd, bool success)
    : succeed_or_fail_call_info (cd, success)
    {}

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      const call_details cd (get_call_details (model, ctxt));

      if (m_success)
	{
	  /* Return 1.  */
	  model->update_for_int_cst_return (cd, 1, true);
	}
      else
	{
	  /* Return 0; set errno.  */
	  model->update_for_int_cst_return (cd, 0, true);
	  model->set_errno (cd);
	}

      return feasible_p (cd, ctxt);
    }

  private:
    bool feasible_p (const call_details &cd,
		     region_model_context *ctxt) const
    {
      if (m_success)
	{
	  /* Can't be "success" on a closed/invalid fd.  */
	  sm_state_map *smap;
	  const fd_state_machine *fd_sm;
	  std::unique_ptr<sm_context> sm_ctxt;
	  if (!get_fd_state (ctxt, &smap, &fd_sm, NULL, &sm_ctxt))
	    return true;
	  const extrinsic_state *ext_state = ctxt->get_ext_state ();
	  if (!ext_state)
	    return true;

	  const svalue *fd_sval = cd.get_arg_svalue (0);
	  state_machine::state_t old_state
	    = sm_ctxt->get_state (cd.get_call_stmt (), fd_sval);

	  if (fd_sm->is_closed_fd_p (old_state)
	      || old_state == fd_sm->m_invalid)
	    return false;
	}
      return true;
    }
  }; // class outcome_of_isatty

public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return cd.num_args () == 1;
  }

  void impl_call_post (const call_details &cd) const final override
  {
    if (cd.get_ctxt ())
      {
	cd.get_ctxt ()->bifurcate (make_unique<outcome_of_isatty> (cd, false));
	cd.get_ctxt ()->bifurcate (make_unique<outcome_of_isatty> (cd, true));
	cd.get_ctxt ()->terminate_path ();
      }
  }
};

/* Handler for calls to "pipe" and "pipe2".
   See e.g. https://www.man7.org/linux/man-pages/man2/pipe.2.html  */

class kf_pipe : public known_function
{
  class failure : public failed_call_info
  {
  public:
    failure (const call_details &cd) : failed_call_info (cd) {}

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      /* Return -1; everything else is unchanged.  */
      const call_details cd (get_call_details (model, ctxt));
      model->update_for_int_cst_return (cd, -1, true);
      return true;
    }
  };

  class success : public success_call_info
  {
  public:
    success (const call_details &cd) : success_call_info (cd) {}

    bool update_model (region_model *model,
		       const exploded_edge *,
		       region_model_context *ctxt) const final override
    {
      const call_details cd (get_call_details (model, ctxt));

      /* Return 0.  */
      model->update_for_zero_return (cd, true);

      /* Update fd array.  */
      region_model_manager *mgr = cd.get_manager ();
      tree arr_tree = cd.get_arg_tree (0);
      const svalue *arr_sval = cd.get_arg_svalue (0);
      for (int idx = 0; idx < 2; idx++)
	{
	  const region *arr_reg
	    = model->deref_rvalue (arr_sval, arr_tree, cd.get_ctxt ());
	  const svalue *idx_sval
	    = mgr->get_or_create_int_cst (integer_type_node, idx);
	  const region *element_reg
	    = mgr->get_element_region (arr_reg, integer_type_node, idx_sval);
	  conjured_purge p (model, cd.get_ctxt ());
	  const svalue *fd_sval
	    = mgr->get_or_create_conjured_svalue (integer_type_node,
						  cd.get_call_stmt (),
						  element_reg,
						  p);
	  model->set_value (element_reg, fd_sval, cd.get_ctxt ());
	  model->mark_as_valid_fd (fd_sval, cd.get_ctxt ());
	}
      return true;
    }
  };

public:
  kf_pipe (unsigned num_args)
  : m_num_args (num_args)
  {
    gcc_assert (num_args > 0);
  }

  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == m_num_args && cd.arg_is_pointer_p (0));
  }

  void impl_call_post (const call_details &cd) const final override
  {
    if (cd.get_ctxt ())
      {
	cd.get_ctxt ()->bifurcate (make_unique<failure> (cd));
	cd.get_ctxt ()->bifurcate (make_unique<success> (cd));
	cd.get_ctxt ()->terminate_path ();
      }
  }

private:
  unsigned m_num_args;
};

/* Handler for "read".
     ssize_t read(int fildes, void *buf, size_t nbyte);
   See e.g. https://man7.org/linux/man-pages/man2/read.2.html   */

class kf_read : public known_function
{
public:
  bool matches_call_types_p (const call_details &cd) const final override
  {
    return (cd.num_args () == 3
	    && cd.arg_is_pointer_p (1)
	    && cd.arg_is_size_p (2));
  }

  /* For now, assume that any call to "read" fully clobbers the buffer
     passed in.  This isn't quite correct (e.g. errors, partial reads;
     see PR analyzer/108689), but at least stops us falsely complaining
     about the buffer being uninitialized.  */
  void impl_call_pre (const call_details &cd) const final override
  {
    region_model *model = cd.get_model ();
    const svalue *ptr_sval = cd.get_arg_svalue (1);
    if (const region *reg = ptr_sval->maybe_get_region ())
      {
	const region *base_reg = reg->get_base_region ();
	const svalue *new_sval = cd.get_or_create_conjured_svalue (base_reg);
	model->set_value (base_reg, new_sval, cd.get_ctxt ());
      }
    cd.set_any_lhs_with_defaults ();
  }
};


/* Populate KFM with instances of known functions relating to
   file descriptors.  */

void
register_known_fd_functions (known_function_manager &kfm)
{
  kfm.add ("accept", make_unique<kf_accept> ());
  kfm.add ("bind", make_unique<kf_bind> ());
  kfm.add ("connect", make_unique<kf_connect> ());
  kfm.add ("isatty", make_unique<kf_isatty> ());
  kfm.add ("listen", make_unique<kf_listen> ());
  kfm.add ("pipe", make_unique<kf_pipe> (1));
  kfm.add ("pipe2", make_unique<kf_pipe> (2));
  kfm.add ("read", make_unique<kf_read> ());
  kfm.add ("socket", make_unique<kf_socket> ());
}

} // namespace ana

#endif // ENABLE_ANALYZER
