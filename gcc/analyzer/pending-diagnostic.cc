/* Classes for analyzer diagnostics.
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

#include "config.h"
#define INCLUDE_MEMORY
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "intl.h"
#include "diagnostic.h"
#include "analyzer/analyzer.h"
#include "diagnostic-event-id.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/sm.h"
#include "diagnostic-event-id.h"
#include "analyzer/sm.h"
#include "analyzer/pending-diagnostic.h"
#include "analyzer/diagnostic-manager.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "cpplib.h"
#include "digraph.h"
#include "ordered-hash-map.h"
#include "cfg.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "cgraph.h"
#include "analyzer/supergraph.h"
#include "analyzer/program-state.h"
#include "analyzer/exploded-graph.h"
#include "diagnostic-path.h"
#include "analyzer/checker-path.h"
#include "make-unique.h"

#if ENABLE_ANALYZER

namespace ana {

/* struct interesting_t.  */

/* Mark the creation of REG as being interesting.  */

void
interesting_t::add_region_creation (const region *reg)
{
  gcc_assert (reg);
  m_region_creation.safe_push (reg);
}

void
interesting_t::dump_to_pp (pretty_printer *pp, bool simple) const
{
  pp_string (pp, "{ region creation: [");
  unsigned i;
  const region *reg;
  FOR_EACH_VEC_ELT (m_region_creation, i, reg)
    {
      if (i > 0)
	pp_string (pp, ", ");
      reg->dump_to_pp (pp, simple);
    }
  pp_string (pp, "]}");
}

/* Generate a label_text by printing FMT.

   Use a clone of the global_dc for formatting callbacks.

   Use this evdesc::event_desc's m_colorize flag to control colorization
   (so that e.g. we can disable it for JSON output).  */

label_text
evdesc::event_desc::formatted_print (const char *fmt, ...) const
{
  pretty_printer *pp = global_dc->printer->clone ();

  pp_show_color (pp) = m_colorize;

  rich_location rich_loc (line_table, UNKNOWN_LOCATION);
  va_list ap;
  va_start (ap, fmt);
  text_info ti (_(fmt), &ap, 0, nullptr, &rich_loc);
  pp_format (pp, &ti);
  pp_output_formatted_text (pp);
  va_end (ap);

  label_text result = label_text::take (xstrdup (pp_formatted_text (pp)));
  delete pp;
  return result;
}

/* class diagnostic_emission_context.  */

/* Get the pending_diagnostic being emitted.  */

const pending_diagnostic &
diagnostic_emission_context::get_pending_diagnostic () const
{
  return *m_sd.m_d.get ();
}

/* Emit a warning, using the rich_location, metadata, and the
   pending_diagnostic's option.  */

bool
diagnostic_emission_context::warn (const char *gmsgid, ...)
{
  const pending_diagnostic &pd = get_pending_diagnostic ();
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  const bool result = emit_diagnostic_valist_meta (DK_WARNING,
						   &m_rich_loc, &m_metadata,
						   pd.get_controlling_option (),
						   gmsgid, &ap);
  va_end (ap);
  return result;
}

/* Emit a note, using the rich_location and metadata (and the
   pending_diagnostic's option).  */

void
diagnostic_emission_context::inform (const char *gmsgid, ...)
{
  const pending_diagnostic &pd = get_pending_diagnostic ();
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  emit_diagnostic_valist_meta (DK_NOTE,
			       &m_rich_loc, &m_metadata,
			       pd.get_controlling_option (),
			       gmsgid, &ap);
  va_end (ap);
}

/* Return true if T1 and T2 are "the same" for the purposes of
   diagnostic deduplication.  */

bool
pending_diagnostic::same_tree_p (tree t1, tree t2)
{
  return simple_cst_equal (t1, t2) == 1;
}

/* Return true iff IDENT is STR.  */

static bool
ht_ident_eq (ht_identifier ident, const char *str)
{
  return (strlen (str) == ident.len
	  && 0 == strcmp (str, (const char *)ident.str));
}

/* Return true if we should show the expansion location rather than unwind
   within MACRO.  */

static bool
fixup_location_in_macro_p (cpp_hashnode *macro)
{
  ht_identifier ident = macro->ident;

  /* Don't unwind inside "alloca" macro, so that we don't suppress warnings
     from it (due to being in system headers).  */
  if (ht_ident_eq (ident, "alloca"))
    return true;

  /* Don't unwind inside <stdarg.h> macros, so that we don't suppress warnings
     from them (due to being in system headers).  */
  if (ht_ident_eq (ident, "va_start")
      || ht_ident_eq (ident, "va_copy")
      || ht_ident_eq (ident, "va_arg")
      || ht_ident_eq (ident, "va_end"))
    return true;
  return false;
}

/* Base implementation of pending_diagnostic::fixup_location.
   Don't unwind inside macros for which fixup_location_in_macro_p is true.  */

location_t
pending_diagnostic::fixup_location (location_t loc, bool) const
{
  if (linemap_location_from_macro_expansion_p (line_table, loc))
    {
      line_map *map
	= const_cast <line_map *> (linemap_lookup (line_table, loc));
      const line_map_macro *macro_map = linemap_check_macro (map);
      if (fixup_location_in_macro_p (macro_map->macro))
	loc = linemap_resolve_location (line_table, loc,
					LRK_MACRO_EXPANSION_POINT, NULL);
    }
  return loc;
}

/* Base implementation of pending_diagnostic::add_function_entry_event.
   Add a function_entry_event to EMISSION_PATH.  */

void
pending_diagnostic::add_function_entry_event (const exploded_edge &eedge,
					      checker_path *emission_path)
{
  const exploded_node *dst_node = eedge.m_dest;
  const program_point &dst_point = dst_node->get_point ();
  emission_path->add_event (make_unique<function_entry_event> (dst_point));
}

/* Base implementation of pending_diagnostic::add_call_event.
   Add a call_event to EMISSION_PATH.  */

void
pending_diagnostic::add_call_event (const exploded_edge &eedge,
				    checker_path *emission_path)
{
  const exploded_node *src_node = eedge.m_src;
  const program_point &src_point = src_node->get_point ();
  const int src_stack_depth = src_point.get_stack_depth ();
  const gimple *last_stmt = src_point.get_supernode ()->get_last_stmt ();
  emission_path->add_event
    (make_unique<call_event> (eedge,
			      event_loc_info (last_stmt
					      ? last_stmt->location
					      : UNKNOWN_LOCATION,
					      src_point.get_fndecl (),
					      src_stack_depth)));
}

/* Base implementation of pending_diagnostic::add_region_creation_events.
   See the comment for class region_creation_event.  */

void
pending_diagnostic::add_region_creation_events (const region *reg,
						tree capacity,
						const event_loc_info &loc_info,
						checker_path &emission_path)
{
  emission_path.add_event
    (make_unique<region_creation_event_memory_space> (reg->get_memory_space (),
						      loc_info));

  if (capacity)
    emission_path.add_event
      (make_unique<region_creation_event_capacity> (capacity, loc_info));
}

/* Base implementation of pending_diagnostic::add_final_event.
   Add a warning_event to the end of EMISSION_PATH.  */

void
pending_diagnostic::add_final_event (const state_machine *sm,
				     const exploded_node *enode,
				     const gimple *stmt,
				     tree var, state_machine::state_t state,
				     checker_path *emission_path)
{
  emission_path->add_event
    (make_unique<warning_event>
     (event_loc_info (get_stmt_location (stmt, enode->get_function ()),
		      enode->get_function ()->decl,
		      enode->get_stack_depth ()),
      enode,
      sm, var, state));
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
