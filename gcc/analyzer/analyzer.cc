/* Utility functions for the analyzer.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.
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
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "diagnostic.h"
#include "intl.h"
#include "function.h"
#include "analyzer/analyzer.h"

#if ENABLE_ANALYZER

/* Helper function for checkers.  Is the CALL to the given function name,
   and with the given number of arguments?

   This doesn't resolve function pointers via the region model;
   is_named_call_p should be used instead, using a fndecl from
   get_fndecl_for_call; this function should only be used for special cases
   where it's not practical to get at the region model, or for special
   analyzer functions such as __analyzer_dump.  */

bool
is_special_named_call_p (const gcall *call, const char *funcname,
			 unsigned int num_args)
{
  gcc_assert (funcname);

  tree fndecl = gimple_call_fndecl (call);
  if (!fndecl)
    return false;

  return is_named_call_p (fndecl, funcname, call, num_args);
}

/* Helper function for checkers.  Does FNDECL have the given FUNCNAME?  */

bool
is_named_call_p (tree fndecl, const char *funcname)
{
  gcc_assert (fndecl);
  gcc_assert (funcname);

  return 0 == strcmp (IDENTIFIER_POINTER (DECL_NAME (fndecl)), funcname);
}

/* Helper function for checkers.  Does FNDECL have the given FUNCNAME, and
   does CALL have the given number of arguments?  */

bool
is_named_call_p (tree fndecl, const char *funcname,
		 const gcall *call, unsigned int num_args)
{
  gcc_assert (fndecl);
  gcc_assert (funcname);

  if (!is_named_call_p (fndecl, funcname))
    return false;

  if (gimple_call_num_args (call) != num_args)
    return false;

  return true;
}

/* Return true if stmt is a setjmp call.  */

bool
is_setjmp_call_p (const gimple *stmt)
{
  /* TODO: is there a less hacky way to check for "setjmp"?  */
  if (const gcall *call = dyn_cast <const gcall *> (stmt))
    if (is_special_named_call_p (call, "setjmp", 1)
	|| is_special_named_call_p (call, "_setjmp", 1))
      return true;

  return false;
}

/* Return true if stmt is a longjmp call.  */

bool
is_longjmp_call_p (const gcall *call)
{
  /* TODO: is there a less hacky way to check for "longjmp"?  */
  if (is_special_named_call_p (call, "longjmp", 2))
    return true;

  return false;
}

/* Generate a label_text instance by formatting FMT, using a
   temporary clone of the global_dc's printer (thus using its
   formatting callbacks).

   Colorize if the global_dc supports colorization and CAN_COLORIZE is
   true.  */

label_text
make_label_text (bool can_colorize, const char *fmt, ...)
{
  pretty_printer *pp = global_dc->printer->clone ();
  pp_clear_output_area (pp);

  if (!can_colorize)
    pp_show_color (pp) = false;

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

#endif /* #if ENABLE_ANALYZER */
