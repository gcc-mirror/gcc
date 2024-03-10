/* ACLE support for Arm MVE (__ARM_FEATURE_MVE intrinsics)
   Copyright (C) 2023 Free Software Foundation, Inc.

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

#ifndef GCC_ARM_MVE_BUILTINS_BASE_H
#define GCC_ARM_MVE_BUILTINS_BASE_H

namespace arm_mve {
namespace functions {

extern const function_base *const vaddq;
extern const function_base *const vandq;
extern const function_base *const vcreateq;
extern const function_base *const veorq;
extern const function_base *const vhaddq;
extern const function_base *const vhsubq;
extern const function_base *const vmulhq;
extern const function_base *const vmulq;
extern const function_base *const vorrq;
extern const function_base *const vqaddq;
extern const function_base *const vqdmulhq;
extern const function_base *const vqsubq;
extern const function_base *const vreinterpretq;
extern const function_base *const vrhaddq;
extern const function_base *const vrmulhq;
extern const function_base *const vsubq;
extern const function_base *const vuninitializedq;

} /* end namespace arm_mve::functions */
} /* end namespace arm_mve */

#endif
