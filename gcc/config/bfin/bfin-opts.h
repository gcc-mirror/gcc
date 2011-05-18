/* Definitions for the Blackfin port needed for option handling.
   Copyright (C) 2005, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.

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

#ifndef BFIN_OPTS_H
#define BFIN_OPTS_H

/* CPU type.  */
typedef enum bfin_cpu_type
{
  BFIN_CPU_UNKNOWN,
  BFIN_CPU_BF512,
  BFIN_CPU_BF514,
  BFIN_CPU_BF516,
  BFIN_CPU_BF518,
  BFIN_CPU_BF522,
  BFIN_CPU_BF523,
  BFIN_CPU_BF524,
  BFIN_CPU_BF525,
  BFIN_CPU_BF526,
  BFIN_CPU_BF527,
  BFIN_CPU_BF531,
  BFIN_CPU_BF532,
  BFIN_CPU_BF533,
  BFIN_CPU_BF534,
  BFIN_CPU_BF536,
  BFIN_CPU_BF537,
  BFIN_CPU_BF538,
  BFIN_CPU_BF539,
  BFIN_CPU_BF542,
  BFIN_CPU_BF542M,
  BFIN_CPU_BF544,
  BFIN_CPU_BF544M,
  BFIN_CPU_BF547,
  BFIN_CPU_BF547M,
  BFIN_CPU_BF548,
  BFIN_CPU_BF548M,
  BFIN_CPU_BF549,
  BFIN_CPU_BF549M,
  BFIN_CPU_BF561,
  BFIN_CPU_BF592
} bfin_cpu_t;

#endif
