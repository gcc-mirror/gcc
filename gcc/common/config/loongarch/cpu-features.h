/* Definitions of target machine for GNU compiler.  LoongArch version.
   Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by Loongson Ltd.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef LOONGARCH_CPU_FEATURES_H
#define LOONGARCH_CPU_FEATURES_H

typedef unsigned long long loongarch_fmv_feature_mask;

enum CPUFeatures {
  FEAT_LA64,
  FEAT_UAL,
  FEAT_LSX,
  FEAT_LASX,
  FEAT_FRECIPE,
  FEAT_DIV32,
  FEAT_LAM_BH,
  FEAT_LAMCAS,
  FEAT_SCQ,
  FEAT_LD_SEQ_SA,
  FEAT_EXT = 62,
  FEAT_INIT
};

#endif /* LOONGARCH_CPU_FEATURES_H */
