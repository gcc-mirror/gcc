/* Classes for analyzer diagnostics.
   Copyright (C) 2019-2022 Free Software Foundation, Inc.
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

  text_info ti;
  rich_location rich_loc (line_table, UNKNOWN_LOCATION);
  va_list ap;
  va_start (ap, fmt);
  ti.format_spec = _(fmt);
  ti.args_ptr = &ap;
  ti.err_no = 0;
  ti.x_data = NULL;
  ti.m_richloc = &rich_loc;
  pp_format (pp, &ti);
  pp_output_formatted_text (pp);
  va_end (ap);

  label_text result = label_text::take (xstrdup (pp_formatted_text (pp)));
  delete pp;
  return result;
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
pending_diagnostic::fixup_location (location_t loc) const
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
			      (last_stmt
			       ? last_stmt->location
			       : UNKNOWN_LOCATION),
			      src_point.get_fndecl (),
			      src_stack_depth));
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
