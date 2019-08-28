/* Copyright (C) 2016-2019 Free Software Foundation, Inc.

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "memmodel.h"
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
arc64_cpu_cpp_builtins (cpp_reader * pfile)
{
  builtin_assert ("cpu=arc64");
  builtin_assert ("machine=arc64");

  builtin_define ("__ARC64__");
  builtin_define ("__LITTLE_ENDIAN__");
  builtin_define ("__ARCV3__");

  if (arc64_cmodel_var == ARC64_CMODEL_SMALL)
    builtin_define ("__ARC64_CMODEL_SMALL__");
  else if (arc64_cmodel_var == ARC64_CMODEL_MEDIUM)
    builtin_define ("__ARC64_CMODEL_MEDIUM__");
  else if (arc64_cmodel_var == ARC64_CMODEL_LARGE)
    builtin_define ("__ARC64_CMODEL_LARGE__");

  if (TARGET_HARD_FLOAT)
    {
      builtin_define ("__arc_hard_float__");
      builtin_define ("__ARC_HARD_FLOAT__");
      builtin_define ("__ARC_FLOAT_ABI_HARD__");
    }
  else
    {
      builtin_define ("__arc_soft_float__");
      builtin_define ("__ARC_SOFT_FLOAT__");
    }

#undef ARC64_C_DEF
#define ARC64_C_DEF(NAME, CONDITION)		\
  def_or_undef_macro (pfile, NAME, CONDITION);

#include "arc64-c.def"
#undef ARC64_C_DEF
}
