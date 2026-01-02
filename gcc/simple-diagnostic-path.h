/* Concrete classes for implementing diagnostic paths, using tree.
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

#ifndef GCC_SIMPLE_DIAGNOSTIC_PATH_H
#define GCC_SIMPLE_DIAGNOSTIC_PATH_H

#include "diagnostics/paths.h"
#include "tree-logical-location.h"

/* Concrete subclasses of the abstract base classes
   declared in diagnostic-path.h.  */

/* A simple implementation of diagnostic event.
   This uses "tree" and so is not in "namespace diagnostics".  */

class simple_diagnostic_event : public diagnostics::paths::event
{
 public:
  using thread_id_t = diagnostics::paths::thread_id_t;

  simple_diagnostic_event (location_t loc, tree fndecl, int depth,
			   const char *desc,
			   thread_id_t thread_id = 0);
  ~simple_diagnostic_event ();

  location_t get_location () const final override { return m_loc; }
  int get_stack_depth () const final override { return m_depth; }
  void print_desc (pretty_printer &pp) const final override;
  diagnostics::logical_locations::key
  get_logical_location () const final override
  {
    return tree_logical_location_manager::key_from_tree (m_fndecl);
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

  tree get_fndecl () const { return m_fndecl; }

 private:
  location_t m_loc;
  tree m_fndecl;
  diagnostics::logical_locations::key m_logical_loc;
  int m_depth;
  char *m_desc; // has been i18n-ed and formatted
  bool m_connected_to_next_event;
  thread_id_t m_thread_id;
};

/* A simple implementation of diagnostics::paths::thread.  */

class simple_diagnostic_thread : public diagnostics::paths::thread
{
public:
  simple_diagnostic_thread (const char *name) : m_name (name) {}
  label_text get_name (bool) const final override
  {
    return label_text::borrow (m_name);
  }

private:
  const char *m_name; // has been i18n-ed and formatted
};

/* A simple implementation of diagnostic_path, as a vector of
   simple_diagnostic_event instances.  */

class simple_diagnostic_path : public diagnostics::paths::path
{
 public:
  using thread = diagnostics::paths::thread;
  using thread_id_t = diagnostics::paths::thread_id_t;
  using event = diagnostics::paths::event;
  using event_id_t = diagnostics::paths::event_id_t;

  simple_diagnostic_path (const tree_logical_location_manager &logical_loc_mgr,
			  pretty_printer *event_pp);

  unsigned num_events () const final override { return m_events.length (); }
  const event & get_event (int idx) const final override;
  unsigned num_threads () const final override { return m_threads.length (); }
  const thread &
  get_thread (thread_id_t) const final override;
  bool
  same_function_p (int event_idx_a,
		   int event_idx_b) const final override;

  thread_id_t add_thread (const char *name);

  event_id_t add_event (location_t loc, tree fndecl, int depth,
			const char *fmt, ...)
    ATTRIBUTE_GCC_DIAG(5,6);
  event_id_t
  add_thread_event (thread_id_t thread_id,
		    location_t loc, tree fndecl, int depth,
		    const char *fmt, ...)
    ATTRIBUTE_GCC_DIAG(6,7);

  void connect_to_next_event ();

  void disable_event_localization () { m_localize_events = false; }

 private:
  auto_delete_vec<simple_diagnostic_thread> m_threads;
  auto_delete_vec<simple_diagnostic_event> m_events;

  /* (for use by add_event).  */
  pretty_printer *m_event_pp;
  bool m_localize_events;
};

#endif /* ! GCC_SIMPLE_DIAGNOSTIC_PATH_H */
