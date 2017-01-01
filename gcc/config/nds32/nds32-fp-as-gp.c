/* The fp-as-gp pass of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2017 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

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

/* ------------------------------------------------------------------------ */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"

/* ------------------------------------------------------------------------ */

/* Function to determine whether it is worth to do fp_as_gp optimization.
   Return 0: It is NOT worth to do fp_as_gp optimization.
   Return 1: It is APPROXIMATELY worth to do fp_as_gp optimization.
   Note that if it is worth to do fp_as_gp optimization,
   we MUST set FP_REGNUM ever live in this function.  */
int
nds32_fp_as_gp_check_available (void)
{
  /* By default we return 0.  */
  return 0;
}

/* ------------------------------------------------------------------------ */
