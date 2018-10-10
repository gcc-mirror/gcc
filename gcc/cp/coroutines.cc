/* coroutine-specific state, expansions and tests.

   Copyright (C) 2018 Free Software Foundation, Inc.

 Contributed by Iain Sandoe <iain@sandoe.co.uk> under contract to Facebook.

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

/* FIXME: minimise headers.. */
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "bitmap.h"
#include "cp-tree.h"
#include "stringpool.h"
#include "cgraph.h"
#include "stmt.h"
#include "varasm.h"
#include "stor-layout.h"
#include "c-family/c-objc.h"
#include "tree-inline.h"
#include "intl.h"
#include "tree-iterator.h"
#include "omp-general.h"
#include "convert.h"
#include "stringpool.h"
#include "attribs.h"
#include "gomp-constants.h"
#include "predict.h"

/* Check that it's valid to have a co_return keyword here.
   True if this is a valid context (we don't check the content
   of the expr - except to decide if the promise_type needs as
   return_void() or a return_value().  */

bool
co_return_context_valid_p (location_t kw, tree expr)
{
  /* This is arranged in order of prohibitions in the TS.  */
  if (DECL_MAIN_P (current_function_decl))
    {
      // [6.6.1, main shall not be a coroutine].
      error_at (kw, "%<co_return%> cannot be used in"
		" the %<main%> function");
      return false;
    }

  if (DECL_DECLARED_CONSTEXPR_P (current_function_decl))
    {
      // [10.1.5, not constexpr specifier].
      error_at (kw, "%<co_return%> cannot be used in"
		" a %<constexpr%> function");
      cp_function_chain->invalid_constexpr = true;
      return false;
    }

  if (current_function_auto_return_pattern)
    {
      // [10.1.6.4, not auto specifier].
      error_at (kw, "%<co_return%> cannot be used in"
		" a function with a deduced return type");
      return false;
    }

  if (varargs_function_p (current_function_decl))
    {
      // [11.4.4, shall not be varargs].
      error_at (kw, "%<co_return%> cannot be used in"
		" a varargs function");
      return false;
    }

  if (DECL_CONSTRUCTOR_P (current_function_decl))
    {
      // [15.1, A constructor shall not be a coroutine.
      error_at (kw, "%<co_return%> cannot be used in a constructor");
      return false;
    }

  if (DECL_DESTRUCTOR_P (current_function_decl))
    {
      // [15.2, A destructor shall not be a coroutine.
      error_at (kw, "%<co_return%> cannot be used in a destructor");
      return false;
    }

  /* If the promise object doesn't have the correct return call then there's
     a mis-match between the co_return <expr> and this.  It's not clear if 
     this is the right place to diagnose this : FIXME: decide. */
  if (expr == NULL_TREE)
    {
      /* Need to check for the result_void() promise member.  */
    }
   else
    {
      /* Need to check for the result_value() promise member.  */
    }

  /* Makes no sense for a co-routine really. */
  if (TREE_THIS_VOLATILE (current_function_decl))
    warning_at (kw, 0, "function declared %<noreturn%> has a"
		" %<co_return%> statement");

  return true;
}
