/* Various declarations for language-independent diagnostics
   subroutines that are only for use in the compilers proper and not
   the driver or other programs.
   Copyright (C) 2000-2025 Free Software Foundation, Inc.

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

#ifndef GCC_TREE_DIAGNOSTIC_H
#define GCC_TREE_DIAGNOSTIC_H

/* TREE_BLOCK if the diagnostic is to be reported in some inline
   function inlined into other function, otherwise NULL.  */
#define diagnostic_abstract_origin(DI)		\
  ((tree) diagnostic_info_auxiliary_data (DI))

/* Function of last diagnostic message; more generally, function such
   that if next diagnostic message is in it then we don't have to
   mention the function name.  */
#define diagnostic_last_function(DC)			\
  ((tree) diagnostic_context_auxiliary_data (DC))

/* True if the last function in which a diagnostic was reported is
   different from the current one.  */
#define diagnostic_last_function_changed(DC, DI)			\
  (diagnostic_last_function (DC) != (diagnostic_abstract_origin (DI)	\
				     ? diagnostic_abstract_origin (DI)	\
				     : current_function_decl))

/* Remember the current function as being the last one in which we report
   a diagnostic.  */
#define diagnostic_set_last_function(DC, DI)		\
  diagnostic_context_auxiliary_data (DC)		\
    = (((DI) && diagnostic_abstract_origin (DI))	\
       ? diagnostic_abstract_origin (DI)		\
       : current_function_decl)

void diagnostic_report_current_function (diagnostics::context *,
					 const diagnostics::diagnostic_info *);

void tree_diagnostics_defaults (diagnostics::context *);
bool default_tree_printer (pretty_printer *, text_info *, const char *,
			   int, bool, bool, bool, bool *, pp_token_list &);

/* A subclass of pretty_printer for writing "dump" functions.
   Wires itself up to a FILE *, and colorizes if it's stderr and
   the user requested colorization.  */

class tree_dump_pretty_printer : public pretty_printer
{
public:
  tree_dump_pretty_printer (FILE *outf)
  {
    pp_format_decoder (this) = default_tree_printer;
    if (outf == stderr)
      pp_show_color (this) = pp_show_color (global_dc->get_reference_printer ());
    set_output_stream (outf);
  }
  ~tree_dump_pretty_printer ()
  {
    pp_flush (this);
  }
};

#endif /* ! GCC_TREE_DIAGNOSTIC_H */
