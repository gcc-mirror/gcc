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

extern const function_base *const vabdq;
extern const function_base *const vabsq;
extern const function_base *const vaddq;
extern const function_base *const vandq;
extern const function_base *const vclsq;
extern const function_base *const vclzq;
extern const function_base *const vcreateq;
extern const function_base *const veorq;
extern const function_base *const vhaddq;
extern const function_base *const vhsubq;
extern const function_base *const vmaxavq;
extern const function_base *const vmaxnmq;
extern const function_base *const vmaxq;
extern const function_base *const vmaxvq;
extern const function_base *const vminavq;
extern const function_base *const vminnmq;
extern const function_base *const vminq;
extern const function_base *const vminvq;
extern const function_base *const vmovnbq;
extern const function_base *const vmovntq;
extern const function_base *const vmulhq;
extern const function_base *const vmulq;
extern const function_base *const vnegq;
extern const function_base *const vorrq;
extern const function_base *const vqabsq;
extern const function_base *const vqaddq;
extern const function_base *const vqdmulhq;
extern const function_base *const vqmovnbq;
extern const function_base *const vqmovntq;
extern const function_base *const vqmovunbq;
extern const function_base *const vqmovuntq;
extern const function_base *const vqnegq;
extern const function_base *const vqrdmulhq;
extern const function_base *const vqrshlq;
extern const function_base *const vqrshrnbq;
extern const function_base *const vqrshrntq;
extern const function_base *const vqrshrunbq;
extern const function_base *const vqrshruntq;
extern const function_base *const vqshlq;
extern const function_base *const vqshrnbq;
extern const function_base *const vqshrntq;
extern const function_base *const vqshrunbq;
extern const function_base *const vqshruntq;
extern const function_base *const vqsubq;
extern const function_base *const vreinterpretq;
extern const function_base *const vrhaddq;
extern const function_base *const vrmulhq;
extern const function_base *const vrndaq;
extern const function_base *const vrndmq;
extern const function_base *const vrndnq;
extern const function_base *const vrndpq;
extern const function_base *const vrndq;
extern const function_base *const vrndxq;
extern const function_base *const vrshlq;
extern const function_base *const vrshrnbq;
extern const function_base *const vrshrntq;
extern const function_base *const vrshrq;
extern const function_base *const vshllbq;
extern const function_base *const vshlltq;
extern const function_base *const vshlq;
extern const function_base *const vshrnbq;
extern const function_base *const vshrntq;
extern const function_base *const vshrq;
extern const function_base *const vsubq;
extern const function_base *const vuninitializedq;

} /* end namespace arm_mve::functions */
} /* end namespace arm_mve */

#endif
