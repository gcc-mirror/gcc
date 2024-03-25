/* Subclass of diagnostic_path for analyzer diagnostics.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

namespace ana {

/* Subclass of diagnostic_path for analyzer diagnostics.  */

class checker_path : public diagnostic_path
{
public:
  checker_path (logger *logger)
  : diagnostic_path (),
    m_thread ("main"),
    m_logger (logger)
  {}

  /* Implementation of diagnostic_path vfuncs.  */

  unsigned num_events () const final override
  {
    return m_events.length ();
  }

  const diagnostic_event & get_event (int idx) const final override
  {
    return *m_events[idx];
  }
  unsigned num_threads () const final override
  {
    return 1;
  }
  const diagnostic_thread &
  get_thread (diagnostic_thread_id_t) const final override
  {
    return m_thread;
  }

  checker_event *get_checker_event (int idx)
  {
    return m_events[idx];
  }

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
      e->prepare_for_emission (this, pd, diagnostic_event_id_t (i));
  }

  void fixup_locations (pending_diagnostic *pd);

  void record_setjmp_event (const exploded_node *enode,
			    diagnostic_event_id_t setjmp_emission_id)
  {
    m_setjmp_event_ids.put (enode, setjmp_emission_id);
  }

  bool get_setjmp_event (const exploded_node *enode,
			 diagnostic_event_id_t *out_emission_id)
  {
    if (diagnostic_event_id_t *emission_id = m_setjmp_event_ids.get (enode))
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

  simple_diagnostic_thread m_thread;

  /* The events that have occurred along this path.  */
  auto_delete_vec<checker_event> m_events;

  /* During prepare_for_emission (and after), the setjmp_event for each
     exploded_node *, so that rewind events can refer to them in their
     descriptions.  */
  hash_map <const exploded_node *, diagnostic_event_id_t> m_setjmp_event_ids;

  logger *m_logger;
};

} // namespace ana

#endif /* GCC_ANALYZER_CHECKER_PATH_H */
