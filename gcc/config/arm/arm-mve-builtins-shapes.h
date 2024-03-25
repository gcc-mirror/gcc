/* ACLE support for Arm MVE (function shapes)
   Copyright (C) 2023-2024 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_ARM_MVE_BUILTINS_SHAPES_H
#define GCC_ARM_MVE_BUILTINS_SHAPES_H

namespace arm_mve
{
  /* The naming convention is:

     - to use names like "unary" etc. if the rules are somewhat generic,
       especially if there are no ranges involved.

     Also:

     - "inherent" means that the function takes no arguments.  */

  namespace shapes
  {

    extern const function_shape *const binary;
    extern const function_shape *const binary_acc_int32;
    extern const function_shape *const binary_acc_int64;
    extern const function_shape *const binary_acca_int32;
    extern const function_shape *const binary_acca_int64;
    extern const function_shape *const binary_imm32;
    extern const function_shape *const binary_lshift;
    extern const function_shape *const binary_lshift_r;
    extern const function_shape *const binary_lshift_unsigned;
    extern const function_shape *const binary_maxamina;
    extern const function_shape *const binary_maxavminav;
    extern const function_shape *const binary_maxvminv;
    extern const function_shape *const binary_move_narrow;
    extern const function_shape *const binary_move_narrow_unsigned;
    extern const function_shape *const binary_opt_n;
    extern const function_shape *const binary_orrq;
    extern const function_shape *const binary_round_lshift;
    extern const function_shape *const binary_rshift;
    extern const function_shape *const binary_rshift_narrow;
    extern const function_shape *const binary_rshift_narrow_unsigned;
    extern const function_shape *const binary_widen;
    extern const function_shape *const binary_widen_n;
    extern const function_shape *const binary_widen_opt_n;
    extern const function_shape *const binary_widen_poly;
    extern const function_shape *const cmp;
    extern const function_shape *const create;
    extern const function_shape *const inherent;
    extern const function_shape *const load;
    extern const function_shape *const mvn;
    extern const function_shape *const store;
    extern const function_shape *const ternary;
    extern const function_shape *const ternary_lshift;
    extern const function_shape *const ternary_n;
    extern const function_shape *const ternary_opt_n;
    extern const function_shape *const ternary_rshift;
    extern const function_shape *const unary;
    extern const function_shape *const unary_acc;
    extern const function_shape *const unary_convert;
    extern const function_shape *const unary_int32;
    extern const function_shape *const unary_int32_acc;
    extern const function_shape *const unary_n;
    extern const function_shape *const unary_widen;
    extern const function_shape *const unary_widen_acc;
    extern const function_shape *const vpsel;

  } /* end namespace arm_mve::shapes */
} /* end namespace arm_mve */

#endif
