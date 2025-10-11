/* Debug facilities for the Algol 68 parser.
   Copyright (C) 2025 Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define INCLUDE_MEMORY
#include "config.h"
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "diagnostic.h"
#include "text-art/types.h"
#include "text-art/dump.h"
#include "text-art/dump-widget-info.h"
#include "text-art/canvas.h"
#include "text-art/theme.h"
#include "text-art/tree-widget.h"

#include "a68.h"

/* Write a printable representation of the parse tree with top node P to the
   standard output.  */

static void
a68_dump_parse_tree_1 (NODE_T *p, const text_art::dump_widget_info &dwi,
		       text_art::tree_widget &widget, bool tables, bool levels)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      char *symbol;
      if (ATTRIBUTE (p) == IDENTIFIER
	  || ATTRIBUTE (p) == DEFINING_IDENTIFIER
	  || ATTRIBUTE (p) == DEFINING_OPERATOR
	  || ATTRIBUTE (p) == BOLD_TAG)
	symbol = xasprintf (" %s", NSYMBOL (p));
      else
	symbol = xstrdup ("");

      char *tableinfo;
      if (tables && TABLE (p) != NO_TABLE)
	tableinfo = xasprintf (" table=%p prev=%p",
			       (void *) TABLE (p),
			       (void *) PREVIOUS (TABLE (p)));
      else
	tableinfo = xstrdup ("");

      char *levelsinfo;
      if (levels && TABLE (p) != NO_TABLE)
	levelsinfo = xasprintf (" level=%d", LEVEL (TABLE (p)));
      else
	levelsinfo = xstrdup ("");

      char mode[BUFFER_SIZE];
      mode[0] = '\0';
      if (MOID (p) != NO_MOID)
	{
	  MOID_T *moid = MOID (p);
	  mode[0] = '\0';

	  a68_bufcat (mode, " (", 2);
	  if (IS (moid, SERIES_MODE))
	    {
	      if (PACK (moid) != NO_PACK && NEXT (PACK (moid)) == NO_PACK)
		a68_bufcat (mode, a68_moid_to_string (MOID (PACK (moid)), MOID_ERROR_WIDTH, p),
			    BUFFER_SIZE);
	      else
		a68_bufcat (mode, a68_moid_to_string (moid, MOID_ERROR_WIDTH, p), BUFFER_SIZE);
	    }
	  else
	    a68_bufcat (mode, a68_moid_to_string (moid, MOID_ERROR_WIDTH, p), BUFFER_SIZE);
	  a68_bufcat (mode, ")", 2);
	}

      location_t loc = a68_get_node_location (p);
      std::unique_ptr<text_art::tree_widget> cwidget
	= text_art::tree_widget::from_fmt (dwi, nullptr,
					   "%s:%d:%d [%d] %s%s%s%s%s",
					   LOCATION_FILE (loc),
					   LOCATION_LINE (loc),
					   LOCATION_COLUMN (loc),
					   NUMBER (p),
					   a68_attribute_name (ATTRIBUTE (p)),
					   symbol,
					   mode,
					   tableinfo,
					   levelsinfo);
      free (symbol);
      free (tableinfo);
      free (levelsinfo);

      a68_dump_parse_tree_1 (SUB (p), dwi, *cwidget, tables, levels);
      widget.add_child (std::move (cwidget));
    }
}

void
a68_dump_parse_tree (NODE_T *p, bool tables, bool levels)
{
  text_art::style_manager sm;
  text_art::style::id_t default_style_id (sm.get_or_create_id (text_art::style ()));
  text_art::ascii_theme theme;
  text_art::dump_widget_info dwi (sm, theme, default_style_id);
  std::unique_ptr<text_art::tree_widget> widget
    = text_art::tree_widget::from_fmt (dwi, nullptr, "Parse Tree");

  a68_dump_parse_tree_1 (p, dwi, *widget, tables, levels);

  text_art::canvas c (widget->to_canvas (sm));
  pretty_printer *const pp = global_dc->get_reference_printer ();
  pp_clear_output_area (pp);
  c.print_to_pp (pp);
  printf ("%s", pp_formatted_text (pp));
}

/* Dump the modes in the list MOID.  */

void
a68_dump_modes (MOID_T *moid)
{
  for (; moid != NO_MOID; FORWARD (moid))
    {
      printf ("%p %s\n", (void *) moid,
	      a68_moid_to_string (moid, MOID_ERROR_WIDTH, NODE (moid),
				  true /* indicant_value */));
    }
}

/* Dump a given MOIF.  */

void
a68_dump_moif (MOIF_T *moif)
{
  text_art::style_manager sm;
  text_art::style::id_t default_style_id (sm.get_or_create_id (text_art::style ()));
  text_art::ascii_theme theme;
  text_art::dump_widget_info dwi (sm, theme, default_style_id);
  std::unique_ptr<text_art::tree_widget> widget
    = text_art::tree_widget::from_fmt (dwi, nullptr, "module interface: %s", NAME (moif));

  std::unique_ptr<text_art::tree_widget> prelude_widget
    = text_art::tree_widget::from_fmt (dwi, nullptr, "prelude: %s", PRELUDE (moif));
  widget->add_child (std::move (prelude_widget));

  std::unique_ptr<text_art::tree_widget> postlude_widget
    = text_art::tree_widget::from_fmt (dwi, nullptr, "postlude: %s", POSTLUDE (moif));
  widget->add_child (std::move (postlude_widget));

  /* Mode table.  */
  std::unique_ptr<text_art::tree_widget> modes_widget
    = text_art::tree_widget::from_fmt (dwi, nullptr, "modes");
  for (MOID_T *mode : MODES (moif))
    {
      const char *asm_label = ASM_LABEL (mode);
      if (asm_label == NULL)
	asm_label = a68_moid_to_string (mode, 80, NO_NODE, false);
      std::unique_ptr<text_art::tree_widget> mode_widget
	= text_art::tree_widget::from_fmt (dwi, nullptr, "mode: %s: %s",
					   asm_label,
					   a68_moid_to_string (mode, 80, NO_NODE, true));
      modes_widget->add_child (std::move (mode_widget));
    }
  widget->add_child (std::move (modes_widget));

  /* Module extracts.  */
  std::unique_ptr<text_art::tree_widget> mod_extracts_widget
    = text_art::tree_widget::from_fmt (dwi, nullptr, "module extracts");
  for (EXTRACT_T *e : MODULES (moif))
    {
      std::unique_ptr<text_art::tree_widget> mod_extract_widget
	= text_art::tree_widget::from_fmt (dwi, nullptr, "module extract: %s", EXTRACT_SYMBOL (e));
      mod_extracts_widget->add_child (std::move (mod_extract_widget));
    }
  widget->add_child (std::move (mod_extracts_widget));

  /* Mode extracts.  */
  std::unique_ptr<text_art::tree_widget> mode_extracts_widget
    = text_art::tree_widget::from_fmt (dwi, nullptr, "mode extracts");
  for (EXTRACT_T *e : INDICANTS (moif))
    {
      const char *asm_label = ASM_LABEL (EXTRACT_MODE (e));
      if (asm_label == NULL)
	asm_label = a68_moid_to_string (EXTRACT_MODE (e), 80, NO_NODE, false);
      std::unique_ptr<text_art::tree_widget> mode_extract_widget
	= text_art::tree_widget::from_fmt (dwi, nullptr, "mode extract: %s [%s]",
					   EXTRACT_SYMBOL (e), asm_label);
      mode_extracts_widget->add_child (std::move (mode_extract_widget));
    }
  widget->add_child (std::move (mode_extracts_widget));

  /* Priority extracts.  */
  std::unique_ptr<text_art::tree_widget> prio_extracts_widget
    = text_art::tree_widget::from_fmt (dwi, nullptr, "prio extracts");
  for (EXTRACT_T *e : PRIOS (moif))
    {
      std::unique_ptr<text_art::tree_widget> prio_extract_widget
	= text_art::tree_widget::from_fmt (dwi, nullptr, "prio extract: %s [prio: %d]",
					   EXTRACT_SYMBOL (e), EXTRACT_PRIO (e));
      prio_extracts_widget->add_child (std::move (prio_extract_widget));
    }
  widget->add_child (std::move (prio_extracts_widget));

  /* Identifier extracts.  */
  std::unique_ptr<text_art::tree_widget> id_extracts_widget
    = text_art::tree_widget::from_fmt (dwi, nullptr, "identifier extracts");
  for (EXTRACT_T *e : IDENTIFIERS (moif))
    {
      const char *asm_label = ASM_LABEL (EXTRACT_MODE (e));
      if (asm_label == NULL)
	asm_label = a68_moid_to_string (EXTRACT_MODE (e), 80, NO_NODE, false);
      std::unique_ptr<text_art::tree_widget> id_extract_widget
	= text_art::tree_widget::from_fmt (dwi, nullptr, "iden extract: %s [%s] variable=%d inproc=%d",
					   EXTRACT_SYMBOL (e), asm_label,
					   EXTRACT_VARIABLE (e),
					   EXTRACT_IN_PROC (e));
      id_extracts_widget->add_child (std::move (id_extract_widget));
    }
  widget->add_child (std::move (id_extracts_widget));

  /* Operator extracts.  */
  std::unique_ptr<text_art::tree_widget> op_extracts_widget
    = text_art::tree_widget::from_fmt (dwi, nullptr, "operator extracts");
  for (EXTRACT_T *e : OPERATORS (moif))
    {
      const char *asm_label = ASM_LABEL (EXTRACT_MODE (e));
      if (asm_label == NULL)
	asm_label = a68_moid_to_string (EXTRACT_MODE (e), 80, NO_NODE, false);

      std::unique_ptr<text_art::tree_widget> op_extract_widget
	= text_art::tree_widget::from_fmt (dwi, nullptr, "op extract: %s [%s] variable=%d inproc=%d",
					   EXTRACT_SYMBOL (e), asm_label,
					   EXTRACT_VARIABLE (e),
					   EXTRACT_IN_PROC (e));
      op_extracts_widget->add_child (std::move (op_extract_widget));
    }
  widget->add_child (std::move (op_extracts_widget));

  text_art::canvas c (widget->to_canvas (sm));
  pretty_printer *const pp = global_dc->get_reference_printer ();
  pp_clear_output_area (pp);
  c.print_to_pp (pp);
  printf ("%s", pp_formatted_text (pp));
}
