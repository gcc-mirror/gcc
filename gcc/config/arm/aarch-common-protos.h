/* Function prototypes for instruction scheduling dependeoncy routines,
   defined in aarch-common.c

   Copyright (C) 1991-2013 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

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


#ifndef GCC_AARCH_COMMON_PROTOS_H
#define GCC_AARCH_COMMON_PROTOS_H

extern int arm_early_load_addr_dep (rtx, rtx);
extern int arm_early_store_addr_dep (rtx, rtx);
extern int arm_mac_accumulator_is_mul_result (rtx, rtx);
extern int arm_mac_accumulator_is_result (rtx, rtx);
extern int arm_no_early_alu_shift_dep (rtx, rtx);
extern int arm_no_early_alu_shift_value_dep (rtx, rtx);
extern int arm_no_early_mul_dep (rtx, rtx);
extern int arm_no_early_store_addr_dep (rtx, rtx);

#endif /* GCC_AARCH_COMMON_PROTOS_H */
