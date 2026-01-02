/* Subclass of diagnostics::paths::path for analyzer diagnostics.
   Copyright (C) 2019-2026 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_CHECKER_PATH_H
#define GCC_ANALYZER_CHECKER_PATH_H

#include "analyzer/checker-event.h"
#include "simple-diagnostic-path.h"

namespace ana {

/* Subclass of diagnostic path for analyzer diagnostics.  */

class checker_path : public diagnostics::paths::path
{
public:
  checker_path (const diagnostics::logical_locations::manager &logical_loc_mgr,
		const extrinsic_state &ext_state,
		logger *logger)
  : diagnostics::paths::path (logical_loc_mgr),
    m_ext_state (ext_state),
    m_thread ("main"),
    m_logger (logger)
  {}

  /* Implementation of diagnostics::paths::path vfuncs.  */

  unsigned num_events () const final override
  {
    return m_events.length ();
  }

  const diagnostics::paths::event &
  get_event (int idx) const final override
  {
    return *m_events[idx];
  }
  unsigned num_threads () const final override
  {
    return 1;
  }
  const diagnostics::paths::thread &
  get_thread (diagnostics::paths::thread_id_t) const final override
  {
    return m_thread;
  }

  const extrinsic_state &get_ext_state () const { return m_ext_state; }

  checker_event *get_checker_event (int idx)
  {
    return m_events[idx];
  }

  bool
  same_function_p (int event_idx_a,
		   int event_idx_b) const final override;

  void dump (pretty_printer *pp) const;
  void debug () const;

  logger *get_logger () const { return m_logger; }
  void maybe_log (logger *logger, const char *desc) const;

  void add_event (std::unique_ptr<checker_event> event);

  void delete_event (int idx)
  {
    checker_event *event = m_events[idx];
    m_events.ordered_remove (idx);
    delete event;
  }

  void delete_events (unsigned start_idx, unsigned len)
  {
    for (unsigned i = start_idx; i < start_idx + len; i++)
      delete m_events[i];
    m_events.block_remove (start_idx, len);
  }

  void replace_event (unsigned idx, checker_event *new_event)
  {
    delete m_events[idx];
    m_events[idx] = new_event;
  }

  void add_region_creation_events (pending_diagnostic *pd,
				   const region *reg,
				   const region_model *model,
				   const event_loc_info &loc_info,
				   bool debug);

  /* After all event-pruning, a hook for notifying each event what
     its ID will be.  The events are notified in order, allowing
     for later events to refer to the IDs of earlier events in
     their descriptions.  */
  void prepare_for_emission (pending_diagnostic *pd)
  {
    checker_event *e;
    int i;
    FOR_EACH_VEC_ELT (m_events, i, e)
      e->prepare_for_emission (this, pd, diagnostics::paths::event_id_t (i));
  }

  void fixup_locations (pending_diagnostic *pd);

  void record_setjmp_event (const exploded_node *enode,
			    diagnostics::paths::event_id_t setjmp_emission_id)
  {
    m_setjmp_event_ids.put (enode, setjmp_emission_id);
  }

  bool get_setjmp_event (const exploded_node *enode,
			 diagnostics::paths::event_id_t *out_emission_id)
  {
    if (diagnostics::paths::event_id_t *emission_id
	  = m_setjmp_event_ids.get (enode))
      {
	*out_emission_id = *emission_id;
	return true;
      }
    return false;
  }

  bool cfg_edge_pair_at_p (unsigned idx) const;

  void inject_any_inlined_call_events (logger *logger);

private:
  DISABLE_COPY_AND_ASSIGN(checker_path);

  const extrinsic_state &m_ext_state;

  simple_diagnostic_thread m_thread;

  /* The events that have occurred along this path.  */
  auto_delete_vec<checker_event> m_events;

  /* During prepare_for_emission (and after), the setjmp_event for each
     exploded_node *, so that rewind events can refer to them in their
     descriptions.  */
  hash_map <const exploded_node *, diagnostics::paths::event_id_t> m_setjmp_event_ids;

  logger *m_logger;
};

} // namespace ana

#endif /* GCC_ANALYZER_CHECKER_PATH_H */
