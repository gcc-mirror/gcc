/* Default target hook functions.
   Copyright (C) 2003 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "machmode.h"
#include "rtl.h"
#include "tree.h"
#include "expr.h"
#include "toplev.h"
#include "function.h"
#include "target.h"
#include "tm_p.h"
#include "target-def.h"

bool
default_promote_function_args (fntype)
     tree fntype ATTRIBUTE_UNUSED;
{
#ifdef PROMOTE_FUNCTION_ARGS
  return true;
#else
  return false;
#endif
}

bool
default_promote_function_return (fntype)
     tree fntype ATTRIBUTE_UNUSED;
{
#ifdef PROMOTE_FUNCTION_RETURN
  return true;
#else
  return false;
#endif
}

bool
default_promote_prototypes (fntype)
     tree fntype ATTRIBUTE_UNUSED;
{
  if (PROMOTE_PROTOTYPES)
    return true;
  else
    return false;
}

rtx
default_struct_value_rtx (tree fntype ATTRIBUTE_UNUSED, int incoming)
{
  rtx rv = 0;
  if (incoming)
    {
#ifdef STRUCT_VALUE_INCOMING
      rv = STRUCT_VALUE_INCOMING;
#else
#ifdef STRUCT_VALUE_INCOMING_REGNUM
      rv = gen_rtx_REG (Pmode, STRUCT_VALUE_INCOMING_REGNUM);
#else
#ifdef STRUCT_VALUE
      rv = STRUCT_VALUE;
#else
#ifndef STRUCT_VALUE_REGNUM
      abort();
#else
      rv = gen_rtx_REG (Pmode, STRUCT_VALUE_REGNUM);
#endif
#endif
#endif
#endif
    }
  else
    {
#ifdef STRUCT_VALUE
      rv = STRUCT_VALUE;
#else
#ifndef STRUCT_VALUE_REGNUM
      abort();
#else
      rv = gen_rtx_REG (Pmode, STRUCT_VALUE_REGNUM);
#endif
#endif
    }
  return rv;
}

bool
default_return_in_memory (tree type,
			  tree fntype ATTRIBUTE_UNUSED)
{
  return (TYPE_MODE (TYPE) == BLKmode);
}

rtx
default_expand_builtin_saveregs (void)
{
#ifdef EXPAND_BUILTIN_SAVEREGS
  return EXPAND_BUILTIN_SAVEREGS ();
#else
  error ("__builtin_saveregs not supported by this target");
  return const0_rtx;
#endif
}

void
default_setup_incoming_varargs (CUMULATIVE_ARGS *ca ATTRIBUTE_UNUSED,
				enum machine_mode mode ATTRIBUTE_UNUSED,
				tree type ATTRIBUTE_UNUSED,
				int *pretend_arg_size ATTRIBUTE_UNUSED,
				int second_time ATTRIBUTE_UNUSED)
{
#ifdef SETUP_INCOMING_VARARGS
  SETUP_INCOMING_VARARGS ((*ca), mode, type, (*pretend_arg_size), second_time);
#endif
}

bool
default_strict_argument_naming (CUMULATIVE_ARGS *ca ATTRIBUTE_UNUSED)
{
#ifdef STRICT_ARGUMENT_NAMING
  return STRICT_ARGUMENT_NAMING;
#else
  return 0;
#endif
}

bool
default_pretend_outgoing_varargs_named(CUMULATIVE_ARGS *ca ATTRIBUTE_UNUSED)
{
#ifdef PRETEND_OUTGOING_VARARGS_NAMED
  return PRETEND_OUTGOING_VARARGS_NAMED;
#else
#ifdef SETUP_INCOMING_VARARGS
  return 1;
#else
  return (targetm.calls.setup_incoming_varargs != default_setup_incoming_varargs);
#endif
#endif
}
