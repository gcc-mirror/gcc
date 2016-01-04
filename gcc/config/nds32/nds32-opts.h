/* Definitions for option handling of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2016 Free Software Foundation, Inc.
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

#ifndef NDS32_OPTS_H
#define NDS32_OPTS_H

#define NDS32_DEFAULT_CACHE_BLOCK_SIZE 16
#define NDS32_DEFAULT_ISR_VECTOR_SIZE (TARGET_ISA_V3 ? 4 : 16)

/* The various ANDES ISA.  */
enum nds32_arch_type
{
  ARCH_V2,
  ARCH_V3,
  ARCH_V3M
};

/* The code model defines the address generation strategy.  */
enum nds32_cmodel_type
{
  CMODEL_SMALL,
  CMODEL_MEDIUM,
  CMODEL_LARGE
};

#endif
