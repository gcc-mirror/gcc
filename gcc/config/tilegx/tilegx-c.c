/* Definitions of C specific functions for TILE-Gx.
   Copyright (C) 2011-2019 Free Software Foundation, Inc.
   Contributed by Walter Lee (walt@tilera.com)

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
#include "tm.h"
#include "c-family/c-common.h"

/* copy defines in c-cppbuiltin.c */
# define builtin_define(TXT) cpp_define (pfile, TXT)
# define builtin_assert(TXT) cpp_assert (pfile, TXT)


/* Implement TARGET_CPU_CPP_BUILTINS.  */
void
tilegx_cpu_cpp_builtins (struct cpp_reader *pfile)
{
  builtin_define ("__tile__");
  builtin_define ("__tilegx__");
  builtin_define ("__tile_chip__=10");
  builtin_define ("__tile_chip_rev__=0");
  builtin_assert ("cpu=tilegx");
  builtin_assert ("machine=tilegx");

  if (TARGET_32BIT)
    builtin_define ("__tilegx32__");

  builtin_define ("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_1");
  builtin_define ("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_2");

  TILEGX_CPU_CPP_ENDIAN_BUILTINS ();
  GNU_USER_TARGET_OS_CPP_BUILTINS ();
}


