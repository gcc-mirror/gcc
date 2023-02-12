/* GCC option-handling definitions for the Synopsys DesignWare ARC architecture.

   Copyright (C) 2007-2023 Free Software Foundation, Inc.

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

#ifndef ARC_OPTS_H
#define ARC_OPTS_H

enum processor_type
{
  PROCESSOR_NONE = 0,
#define ARC_CPU(NAME, ARCH, FLAGS, EXTRA, TUNE)  PROCESSOR_##NAME,
#include "arc-cpus.def"
#undef ARC_CPU
  PROCESSOR_generic
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
/* Quark SE floating point instructions.  */
#define FPX_QK    0x0200

/* fpus option combi.  */
#define FPU_FPUS  (FPU_SP | FPU_SC)
/* fpud option combi.  */
#define FPU_FPUD  (FPU_SP | FPU_SC | FPU_DP | FPU_DC)
/* fpuda option combi.  */
#define FPU_FPUDA (FPU_SP | FPU_SC | FPX_DP)
/* fpuda_div option combi.  */
#define FPU_FPUDA_DIV (FPU_SP | FPU_SC | FPU_SD | FPX_DP)
/* fpuda_fma option combi.  */
#define FPU_FPUDA_FMA (FPU_SP | FPU_SC | FPU_SF | FPX_DP)
/* fpuda_all option combi.  */
#define FPU_FPUDA_ALL (FPU_SP | FPU_SC | FPU_SF | FPU_SD | FPX_DP)
/* fpus_div option combi.  */
#define FPU_FPUS_DIV  (FPU_SP | FPU_SC | FPU_SD)
/* fpus_fma option combi.  */
#define FPU_FPUS_FMA  (FPU_SP | FPU_SC | FPU_SF)
/* fpus_all option combi.  */
#define FPU_FPUS_ALL  (FPU_SP | FPU_SC | FPU_SF | FPU_SD)
/* fpud_div option combi.  */
#define FPU_FPUD_DIV  (FPU_FPUS_DIV | FPU_DP | FPU_DC | FPU_DD)
/* fpud_fma option combi.  */
#define FPU_FPUD_FMA  (FPU_FPUS_FMA | FPU_DP | FPU_DC | FPU_DF)
/* fpud_all option combi.  */
#define FPU_FPUD_ALL  (FPU_FPUS_ALL | FPU_DP | FPU_DC | FPU_DF | FPU_DD)

/* Default FPU option value needed to mark if the variable in question
   is changed by a command line option or not.  This is required when
   we set the cpu's specific configuration.  */
#define DEFAULT_arc_fpu_build 0x10000000

/* Default MPY option value.  */
#define DEFAULT_arc_mpy_option -1

#endif /* ARC_OPTS_H */
