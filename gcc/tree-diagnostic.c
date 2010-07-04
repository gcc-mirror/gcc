/* Language-independent diagnostic subroutines for the GNU Compiler
   Collection that are only for use in the compilers proper and not
   the driver or other programs.
   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
   2009, 2010 Free Software Foundation, Inc.

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
#include "tree-diagnostic.h"
#include "langhooks.h"
#include "langhooks-def.h"

/* Prints out, if necessary, the name of the current function
   that caused an error.  Called from all error and warning functions.  */
void
diagnostic_report_current_function (diagnostic_context *context,
				    diagnostic_info *diagnostic)
{
  diagnostic_report_current_module (context);
  lang_hooks.print_error_function (context, input_filename, diagnostic);
}

void
default_tree_diagnostic_starter (diagnostic_context *context,
				 diagnostic_info *diagnostic)
{
  diagnostic_report_current_function (context, diagnostic);
  pp_set_prefix (context->printer, diagnostic_build_prefix (context,
							    diagnostic));
}
