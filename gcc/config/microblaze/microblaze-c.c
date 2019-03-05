/* Subroutines used for the C front end for Xilinx MicroBlaze.
   Copyright (C) 2010-2019 Free Software Foundation, Inc.

   Contributed by Michael Eager <eager@eagercon.com>.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "c-family/c-common.h"

#define builtin_define(TXT) cpp_define (pfile, TXT)
#define builtin_assert(TXT) cpp_assert (pfile, TXT)

/* Define preprocessor symbols for MicroBlaze.  
   Symbols which do not start with __ are deprecated.  */

void 
microblaze_cpp_define (cpp_reader *pfile)
{
  builtin_assert ("cpu=microblaze");
  builtin_assert ("machine=microblaze");
  builtin_define ("__MICROBLAZE__");
  builtin_define ("__microblaze__");
  if (TARGET_LITTLE_ENDIAN)
    {
      builtin_define ("_LITTLE_ENDIAN");
      builtin_define ("__LITTLE_ENDIAN__");
      builtin_define ("__MICROBLAZEEL__");
    }
  else
    {
      builtin_define ("_BIG_ENDIAN");
      builtin_define ("__BIG_ENDIAN__");
      builtin_define ("__MICROBLAZEEB__");
    }
  if (!TARGET_SOFT_MUL) 
    {
      if (!flag_iso)
        builtin_define ("HAVE_HW_MUL");
      builtin_define ("__HAVE_HW_MUL__");
    }
  if (TARGET_MULTIPLY_HIGH)
    {
      if (!flag_iso)
        builtin_define ("HAVE_HW_MUL_HIGH");
      builtin_define ("__HAVE_HW_MUL_HIGH__");
    }
  if (!TARGET_SOFT_DIV)
    {
      if (!flag_iso)
        builtin_define ("HAVE_HW_DIV");
      builtin_define ("__HAVE_HW_DIV__");
    }
  if (TARGET_BARREL_SHIFT)
    {
      if (!flag_iso)
        builtin_define ("HAVE_HW_BSHIFT");
      builtin_define ("__HAVE_HW_BSHIFT__");
    }
  if (TARGET_PATTERN_COMPARE)
    {
      if (!flag_iso)
        builtin_define ("HAVE_HW_PCMP");
      builtin_define ("__HAVE_HW_PCMP__");
    }
  if (TARGET_HARD_FLOAT)
    {
      if (!flag_iso)
        builtin_define ("HAVE_HW_FPU");
      builtin_define ("__HAVE_HW_FPU__");
    }
  if (TARGET_FLOAT_CONVERT)
    {
      if (!flag_iso)
        builtin_define ("HAVE_HW_FPU_CONVERT");
      builtin_define ("__HAVE_HW_FPU_CONVERT__");
    }
  if (TARGET_FLOAT_SQRT)
    {
      if (!flag_iso)
        builtin_define ("HAVE_HW_FPU_SQRT");
      builtin_define ("__HAVE_HW_FPU_SQRT__");
    }
}  
