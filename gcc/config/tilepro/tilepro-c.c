/* Definitions of C specific functions for TILEPro.
   Copyright (C) 2011, 2012
   Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "machmode.h"
#include "tm.h"
#include "tm_p.h"
#include "cpplib.h"
#include "tree.h"
#include "c-family/c-common.h"

/* copy defines in c-cppbuiltin.c */
# define builtin_define(TXT) cpp_define (pfile, TXT)
# define builtin_assert(TXT) cpp_assert (pfile, TXT)


/* Implement TARGET_CPU_CPP_BUILTINS.  */
void
tilepro_cpu_cpp_builtins (struct cpp_reader *pfile)
{
  builtin_define ("__tile__");
  builtin_define ("__tilepro__");
  builtin_assert ("cpu=tile");
  builtin_assert ("machine=tile");
  builtin_define ("__tile_chip__=1");
  builtin_define ("__tile_chip_rev__=0");

  TILEPRO_CPU_CPP_ENDIAN_BUILTINS ();
  GNU_USER_TARGET_OS_CPP_BUILTINS ();
}


