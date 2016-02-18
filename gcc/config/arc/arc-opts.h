/* GCC option-handling definitions for the Synopsys DesignWare ARC architecture.

   Copyright (C) 2007-2016 Free Software Foundation, Inc.

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

enum processor_type
{
  PROCESSOR_NONE,
  PROCESSOR_ARC600,
  PROCESSOR_ARC601,
  PROCESSOR_ARC700,
  PROCESSOR_ARCEM,
  PROCESSOR_ARCHS
};

/* Single precision floating point.  */
#define FPU_SP    0x0001
/* Single precision fused floating point operations.  */
#define FPU_SF    0x0002
/* Single precision floating point format conversion operations.  */
#define FPU_SC    0x0004
/* Single precision floating point sqrt and div operations.  */
#define FPU_SD    0x0008
/* Double precision floating point.  */
#define FPU_DP    0x0010
/* Double precision fused floating point operations.  */
#define FPU_DF    0x0020
/* Double precision floating point format conversion operations.  */
#define FPU_DC    0x0040
/* Double precision floating point sqrt and div operations.  */
#define FPU_DD    0x0080
/* Double precision floating point assist operations.  */
#define FPX_DP    0x0100

