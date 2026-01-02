/* Concrete classes for selftests involving diagnostic paths.
   Copyright (C) 2019-2026 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_DIAGNOSTICS_SELFTEST_PATHS_H
#define GCC_DIAGNOSTICS_SELFTEST_PATHS_H

#include "diagnostics/paths.h"
#include "diagnostics/selftest-logical-locations.h"

/* The selftest code should entirely disappear in a production
   configuration, hence we guard all of it with #if CHECKING_P.  */

#if CHECKING_P

namespace diagnostics {
namespace paths {
namespace selftest {

/* Concrete subclasses of the abstract base classes
   declared in diagnostic-path.h for use in selftests.

   This code should have no dependency on "tree".  */

/* An implementation of diagnostics::paths::event.  */

class test_event : public event
{
 public:
  using logical_location = logical_locations::key;
  using thread_id_t = paths::thread_id_t;

  test_event (location_t loc,
	      logical_location logical_loc,
	      int depth,
	      const char *desc,
	      thread_id_t thread_id = 0);
  ~test_event ();

  location_t get_location () const final override { return m_loc; }
  int get_stack_depth () const final override { return m_depth; }
  void print_desc (pretty_printer &pp) const final override
  {
    pp_string (&pp, m_desc);
  }
  logical_location get_logical_location () const final override
  {
    return m_logical_loc;
  }
  meaning get_meaning () const final override
  {
    return meaning ();
  }
  bool connect_to_next_event_p () const final override
  {
    return m_connected_to_next_event;
  }
  thread_id_t get_thread_id () const final override
  {
    return m_thread_id;
  }

  void connect_to_next_event ()
  {
    m_connected_to_next_event = true;
  }

 private:
  location_t m_loc;
  logical_location m_logical_loc;
  int m_depth;
  char *m_desc; // has been formatted; doesn't get i18n-ed
  bool m_connected_to_next_event;
  thread_id_t m_thread_id;
};

/* A simple implementation of diagnostics::paths::thread.  */

class test_thread : public thread
{
public:
  test_thread (const char *name) : m_name (name) {}
  label_text get_name (bool) const final override
  {
    return label_text::borrow (m_name);
  }

private:
  const char *m_name; // has been i18n-ed and formatted
};

/* A concrete subclass of diagnostics::paths::path for implementing selftests
   - a vector of test_event instances
   - adds member functions for adding test event
   - does no translation of its events
   - has no dependency on "tree".  */

class test_path : public path
{
 public:
  test_path (logical_locations::selftest::test_manager &logical_loc_mgr,
	     pretty_printer *event_pp);

  unsigned num_events () const final override;
  const event & get_event (int idx) const final override;
  unsigned num_threads () const final override;
  const thread &
  get_thread (thread_id_t) const final override;
  bool
  same_function_p (int event_idx_a,
		   int event_idx_b) const final override;

  thread_id_t add_thread (const char *name);

  event_id_t add_event (location_t loc, const char *funcname, int depth,
			const char *fmt, ...)
    ATTRIBUTE_GCC_DIAG(5,6);
  event_id_t
  add_thread_event (thread_id_t thread_id,
		    location_t loc, const char *funcname, int depth,
		    const char *fmt, ...)
    ATTRIBUTE_GCC_DIAG(6,7);

  void connect_to_next_event ();

  void add_entry (const char *callee_name, int stack_depth,
		  thread_id_t thread_id = 0);
  void add_return (const char *caller_name, int stack_depth,
		   thread_id_t thread_id = 0);
  void add_call (const char *caller_name,
		 int caller_stack_depth,
		 const char *callee_name,
		 thread_id_t thread_id = 0);

 private:
  logical_locations::key
  logical_location_from_funcname (const char *funcname);

  logical_locations::selftest::test_manager &m_test_logical_loc_mgr;
  auto_delete_vec<test_thread> m_threads;
  auto_delete_vec<test_event> m_events;

  /* (for use by add_event).  */
  pretty_printer *m_event_pp;
};

} // namespace diagnostics::paths::selftest
} // namespace diagnostics::paths
} // namespace diagnostics

#endif /* #if CHECKING_P */

#endif /* ! GCC_DIAGNOSTICS_SELFTEST_PATHS_H */
