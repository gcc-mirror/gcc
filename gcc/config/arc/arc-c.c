/* Copyright (C) 2016 Free Software Foundation, Inc.

   This file is part of GCC.

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
   <http://www.gnu.org/licenses/>.
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tm_p.h"
#include "cpplib.h"
#include "c-family/c-common.h"
#include "target.h"

#define builtin_define(TXT) cpp_define (pfile, TXT)
#define builtin_assert(TXT) cpp_assert (pfile, TXT)

/* Define or undefine macros based on the current target.  */

static void
def_or_undef_macro (cpp_reader* pfile, const char *name, bool def_p)
{
  if (def_p)
    cpp_define (pfile, name);
  else
    cpp_undef (pfile, name);
}

/* Helper for TARGET_CPU_CPP_BUILTINS hook.  */

void
arc_cpu_cpp_builtins (cpp_reader * pfile)
{
  builtin_assert ("cpu=arc");
  builtin_assert ("machine=arc");

  builtin_define ("__arc__");

#undef ARC_C_DEF
#define ARC_C_DEF(NAME, CONDITION)		\
  def_or_undef_macro (pfile, NAME, CONDITION);

#include "arc-c.def"
#undef ARC_C_DEF

  builtin_define_with_int_value ("__ARC_TLS_REGNO__",
				 arc_tp_regno);

  builtin_define (TARGET_BIG_ENDIAN
		  ? "__BIG_ENDIAN__" : "__LITTLE_ENDIAN__");
  if (TARGET_BIG_ENDIAN)
    builtin_define ("__big_endian__");

}
