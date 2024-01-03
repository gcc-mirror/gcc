/* ACLE support for Arm MVE (__ARM_FEATURE_MVE intrinsics)
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

#ifndef GCC_ARM_MVE_BUILTINS_BASE_H
#define GCC_ARM_MVE_BUILTINS_BASE_H

namespace arm_mve {
namespace functions {

extern const function_base *const vabavq;
extern const function_base *const vabdq;
extern const function_base *const vabsq;
extern const function_base *const vaddlvaq;
extern const function_base *const vaddlvq;
extern const function_base *const vaddq;
extern const function_base *const vaddvaq;
extern const function_base *const vaddvq;
extern const function_base *const vandq;
extern const function_base *const vbrsrq;
extern const function_base *const vcaddq_rot270;
extern const function_base *const vcaddq_rot90;
extern const function_base *const vclsq;
extern const function_base *const vclzq;
extern const function_base *const vcmlaq;
extern const function_base *const vcmlaq_rot180;
extern const function_base *const vcmlaq_rot270;
extern const function_base *const vcmlaq_rot90;
extern const function_base *const vcmpcsq;
extern const function_base *const vcmpeqq;
extern const function_base *const vcmpgeq;
extern const function_base *const vcmpgtq;
extern const function_base *const vcmphiq;
extern const function_base *const vcmpleq;
extern const function_base *const vcmpltq;
extern const function_base *const vcmpneq;
extern const function_base *const vcmulq;
extern const function_base *const vcmulq_rot180;
extern const function_base *const vcmulq_rot270;
extern const function_base *const vcmulq_rot90;
extern const function_base *const vcreateq;
extern const function_base *const vdupq;
extern const function_base *const veorq;
extern const function_base *const vfmaq;
extern const function_base *const vfmasq;
extern const function_base *const vfmsq;
extern const function_base *const vhaddq;
extern const function_base *const vhcaddq_rot270;
extern const function_base *const vhcaddq_rot90;
extern const function_base *const vhsubq;
extern const function_base *const vld1q;
extern const function_base *const vmaxaq;
extern const function_base *const vmaxavq;
extern const function_base *const vmaxnmaq;
extern const function_base *const vmaxnmavq;
extern const function_base *const vmaxnmq;
extern const function_base *const vmaxnmvq;
extern const function_base *const vmaxq;
extern const function_base *const vmaxvq;
extern const function_base *const vminaq;
extern const function_base *const vminavq;
extern const function_base *const vminnmaq;
extern const function_base *const vminnmavq;
extern const function_base *const vminnmq;
extern const function_base *const vminnmvq;
extern const function_base *const vminq;
extern const function_base *const vminvq;
extern const function_base *const vmladavaq;
extern const function_base *const vmladavaxq;
extern const function_base *const vmladavq;
extern const function_base *const vmladavxq;
extern const function_base *const vmlaldavaq;
extern const function_base *const vmlaldavaxq;
extern const function_base *const vmlaldavq;
extern const function_base *const vmlaldavxq;
extern const function_base *const vmlaq;
extern const function_base *const vmlasq;
extern const function_base *const vmlsdavaq;
extern const function_base *const vmlsdavaxq;
extern const function_base *const vmlsdavq;
extern const function_base *const vmlsdavxq;
extern const function_base *const vmlsldavaq;
extern const function_base *const vmlsldavaxq;
extern const function_base *const vmlsldavq;
extern const function_base *const vmlsldavxq;
extern const function_base *const vmovlbq;
extern const function_base *const vmovltq;
extern const function_base *const vmovnbq;
extern const function_base *const vmovntq;
extern const function_base *const vmulhq;
extern const function_base *const vmullbq_int;
extern const function_base *const vmullbq_poly;
extern const function_base *const vmulltq_int;
extern const function_base *const vmulltq_poly;
extern const function_base *const vmulq;
extern const function_base *const vmvnq;
extern const function_base *const vnegq;
extern const function_base *const vorrq;
extern const function_base *const vpselq;
extern const function_base *const vqabsq;
extern const function_base *const vqaddq;
extern const function_base *const vqdmladhq;
extern const function_base *const vqdmladhxq;
extern const function_base *const vqdmlahq;
extern const function_base *const vqdmlashq;
extern const function_base *const vqdmlsdhq;
extern const function_base *const vqdmlsdhxq;
extern const function_base *const vqdmulhq;
extern const function_base *const vqdmullbq;
extern const function_base *const vqdmulltq;
extern const function_base *const vqmovnbq;
extern const function_base *const vqmovntq;
extern const function_base *const vqmovunbq;
extern const function_base *const vqmovuntq;
extern const function_base *const vqnegq;
extern const function_base *const vqrdmladhq;
extern const function_base *const vqrdmladhxq;
extern const function_base *const vqrdmlahq;
extern const function_base *const vqrdmlashq;
extern const function_base *const vqrdmlsdhq;
extern const function_base *const vqrdmlsdhxq;
extern const function_base *const vqrdmulhq;
extern const function_base *const vqrshlq;
extern const function_base *const vqrshrnbq;
extern const function_base *const vqrshrntq;
extern const function_base *const vqrshrunbq;
extern const function_base *const vqrshruntq;
extern const function_base *const vqshlq;
extern const function_base *const vqshluq;
extern const function_base *const vqshrnbq;
extern const function_base *const vqshrntq;
extern const function_base *const vqshrunbq;
extern const function_base *const vqshruntq;
extern const function_base *const vqsubq;
extern const function_base *const vreinterpretq;
extern const function_base *const vrev16q;
extern const function_base *const vrev32q;
extern const function_base *const vrev64q;
extern const function_base *const vrhaddq;
extern const function_base *const vrmlaldavhaq;
extern const function_base *const vrmlaldavhaxq;
extern const function_base *const vrmlaldavhq;
extern const function_base *const vrmlaldavhxq;
extern const function_base *const vrmlsldavhaq;
extern const function_base *const vrmlsldavhaxq;
extern const function_base *const vrmlsldavhq;
extern const function_base *const vrmlsldavhxq;
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
extern const function_base *const vsliq;
extern const function_base *const vsriq;
extern const function_base *const vst1q;
extern const function_base *const vsubq;
extern const function_base *const vuninitializedq;

} /* end namespace arm_mve::functions */
} /* end namespace arm_mve */

#endif
