/* Language-independent diagnostic subroutines for the GNU Compiler
   Collection that are only for use in the compilers proper and not
   the driver or other programs.
   Copyright (C) 1999-2025 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "diagnostic.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "tree-diagnostic.h"
#include "diagnostics/client-data-hooks.h"
#include "langhooks.h"
#include "intl.h"
#include "diagnostics/text-sink.h"

/* Prints out, if necessary, the name of the current function
   that caused an error.  */
void
diagnostic_report_current_function (diagnostics::text_sink &text_output,
				    const diagnostics::diagnostic_info *diag)
{
  location_t loc = diagnostic_location (diag);
  text_output.report_current_module (loc);
  lang_hooks.print_error_function (text_output, LOCATION_FILE (loc), diag);
}

static void
default_tree_diagnostic_text_starter (diagnostics::text_sink &text_output,
				      const diagnostics::diagnostic_info *diag)
{
  pretty_printer *const pp = text_output.get_printer ();
  diagnostic_report_current_function (text_output, diag);
  pp_set_prefix (pp, text_output.build_prefix (*diag));
}

/* Default tree printer.   Handles declarations only.  */
bool
default_tree_printer (pretty_printer *pp, text_info *text, const char *spec,
		      int precision, bool wide, bool set_locus, bool hash,
		      bool *, pp_token_list &)
{
  tree t;

  /* FUTURE: %+x should set the locus.  */
  if (precision != 0 || wide || hash)
    return false;

  switch (*spec)
    {
    case 'E':
      t = va_arg (*text->m_args_ptr, tree);
      if (TREE_CODE (t) == IDENTIFIER_NODE)
	{
	  pp_identifier (pp, IDENTIFIER_POINTER (t));
	  return true;
	}
      break;

    case 'D':
      t = va_arg (*text->m_args_ptr, tree);
      if (VAR_P (t) && DECL_HAS_DEBUG_EXPR_P (t))
	t = DECL_DEBUG_EXPR (t);
      break;

    case 'F':
    case 'T':
      t = va_arg (*text->m_args_ptr, tree);
      break;

    default:
      return false;
    }

  if (set_locus)
    text->set_location (0, DECL_SOURCE_LOCATION (t), SHOW_RANGE_WITH_CARET);

  if (DECL_P (t))
    {
      const char *n = DECL_NAME (t)
        ? identifier_to_locale (lang_hooks.decl_printable_name (t, 2))
        : _("<anonymous>");
      pp_string (pp, n);
    }
  else
    dump_generic_node (pp, t, 0, TDF_SLIM, 0);

  return true;
}

/* Set the locations of call sites along the inlining stack corresponding
   to the DIAGNOSTIC location.  */

static void
set_inlining_locations (const diagnostics::context &,
			diagnostics::diagnostic_info *diagnostic)
{
  location_t loc = diagnostic_location (diagnostic);
  tree block = LOCATION_BLOCK (loc);

  /* Count the number of locations in system headers.  When all are,
     warnings are suppressed by -Wno-system-headers.  Otherwise, they
     involve some user code, possibly inlined into a function in a system
     header, and are not treated as coming from system headers.  */
  unsigned nsyslocs = 0;

  /* Use a reference to the vector of locations for convenience.  */
  auto &ilocs = diagnostic->m_iinfo.m_ilocs;

  while (block && TREE_CODE (block) == BLOCK
	 && BLOCK_ABSTRACT_ORIGIN (block))
    {
      tree ao = BLOCK_ABSTRACT_ORIGIN (block);
      if (TREE_CODE (ao) == FUNCTION_DECL)
	{
	  if (!diagnostic->m_iinfo.m_ao)
	    diagnostic->m_iinfo.m_ao = block;

	  location_t bsloc = BLOCK_SOURCE_LOCATION (block);
	  ilocs.safe_push (bsloc);
	  if (in_system_header_at (bsloc))
	    ++nsyslocs;
	}
      else if (TREE_CODE (ao) != BLOCK)
	break;

      block = BLOCK_SUPERCONTEXT (block);
    }

  if (ilocs.length ())
    {
      /* When there is an inlining context use the macro expansion
	 location for the original location and bump up NSYSLOCS if
	 it's in a system header since it's not counted above.  */
      location_t sysloc = expansion_point_location_if_in_system_header (loc);
      if (sysloc != loc)
	{
	  loc = sysloc;
	  ++nsyslocs;
	}
    }
  else
    {
      /* When there's no inlining context use the original location
	 and set NSYSLOCS accordingly.  */
      nsyslocs = in_system_header_at (loc) != 0;
    }

  ilocs.safe_push (loc);

  /* Set if all locations are in a system header.  */
  diagnostic->m_iinfo.m_allsyslocs = nsyslocs == ilocs.length ();

  if (tree *ao = pp_ti_abstract_origin (&diagnostic->m_message))
    *ao = (tree)diagnostic->m_iinfo.m_ao;
}

/* Sets CONTEXT to use language independent diagnostics.  */
void
tree_diagnostics_defaults (diagnostics::context *context)
{
  diagnostics::text_starter (context) = default_tree_diagnostic_text_starter;
  diagnostics::text_finalizer (context) = diagnostics::default_text_finalizer;
  context->set_format_decoder (default_tree_printer);
  context->set_set_locations_callback (set_inlining_locations);
  context->set_client_data_hooks (make_compiler_data_hooks ());
}
