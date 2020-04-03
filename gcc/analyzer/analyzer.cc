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

/* Helper function for checkers.  Is FNDECL an extern fndecl at file scope
   that has the given FUNCNAME?

   Compare with special_function_p in calls.c.  */

bool
is_named_call_p (tree fndecl, const char *funcname)
{
  gcc_assert (fndecl);
  gcc_assert (funcname);

  if (!maybe_special_function_p (fndecl))
    return false;

  tree identifier = DECL_NAME (fndecl);
  const char *name = IDENTIFIER_POINTER (identifier);
  const char *tname = name;

  /* Potentially disregard prefix _ or __ in FNDECL's name, but not if
     FUNCNAME itself has leading underscores (e.g. when looking for
     "__analyzer_eval").  */
  if (funcname[0] != '_' && name[0] == '_')
    {
      if (name[1] == '_')
	tname += 2;
      else
	tname += 1;
    }

  return 0 == strcmp (tname, funcname);
}

/* Return true if FNDECL is within the namespace "std".
   Compare with cp/typeck.c: decl_in_std_namespace_p, but this doesn't
   rely on being the C++ FE (or handle inline namespaces inside of std).  */

static inline bool
is_std_function_p (const_tree fndecl)
{
  tree name_decl = DECL_NAME (fndecl);
  if (!name_decl)
    return false;
  if (!DECL_CONTEXT (fndecl))
    return false;
  if (TREE_CODE (DECL_CONTEXT (fndecl)) != NAMESPACE_DECL)
    return false;
  tree ns = DECL_CONTEXT (fndecl);
  if (!(DECL_CONTEXT (ns) == NULL_TREE
	|| TREE_CODE (DECL_CONTEXT (ns)) == TRANSLATION_UNIT_DECL))
    return false;
  if (!DECL_NAME (ns))
    return false;
  return id_equal ("std", DECL_NAME (ns));
}

/* Like is_named_call_p, but look for std::FUNCNAME.  */

bool
is_std_named_call_p (tree fndecl, const char *funcname)
{
  gcc_assert (fndecl);
  gcc_assert (funcname);

  if (!is_std_function_p (fndecl))
    return false;

  tree identifier = DECL_NAME (fndecl);
  const char *name = IDENTIFIER_POINTER (identifier);
  const char *tname = name;

  /* Don't disregard prefix _ or __ in FNDECL's name.  */

  return 0 == strcmp (tname, funcname);
}

/* Helper function for checkers.  Is FNDECL an extern fndecl at file scope
   that has the given FUNCNAME, and does CALL have the given number of
   arguments?  */

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

/* Like is_named_call_p, but check for std::FUNCNAME.  */

bool
is_std_named_call_p (tree fndecl, const char *funcname,
		     const gcall *call, unsigned int num_args)
{
  gcc_assert (fndecl);
  gcc_assert (funcname);

  if (!is_std_named_call_p (fndecl, funcname))
    return false;

  if (gimple_call_num_args (call) != num_args)
    return false;

  return true;
}

/* Return true if stmt is a setjmp or sigsetjmp call.  */

bool
is_setjmp_call_p (const gcall *call)
{
  if (is_special_named_call_p (call, "setjmp", 1)
      || is_special_named_call_p (call, "sigsetjmp", 2))
    return true;

  return false;
}

/* Return true if stmt is a longjmp or siglongjmp call.  */

bool
is_longjmp_call_p (const gcall *call)
{
  if (is_special_named_call_p (call, "longjmp", 2)
      || is_special_named_call_p (call, "siglongjmp", 2))
    return true;

  return false;
}

/* For a CALL that matched is_special_named_call_p or is_named_call_p for
   some name, return a name for the called function suitable for use in
   diagnostics (stripping the leading underscores).  */

const char *
get_user_facing_name (const gcall *call)
{
  tree fndecl = gimple_call_fndecl (call);
  gcc_assert (fndecl);

  tree identifier = DECL_NAME (fndecl);
  gcc_assert (identifier);

  const char *name = IDENTIFIER_POINTER (identifier);

  /* Strip prefix _ or __ in FNDECL's name.  */
  if (name[0] == '_')
    {
      if (name[1] == '_')
	return name + 2;
      else
	return name + 1;
    }

  return name;
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
