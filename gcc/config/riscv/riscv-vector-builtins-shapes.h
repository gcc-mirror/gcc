/* function_shape declaration for RISC-V 'V' Extension for GNU compiler.
   Copyright (C) 2022-2023 Free Software Foundation, Inc.
   Contributed by Ju-Zhe Zhong (juzhe.zhong@rivai.ai), RiVAI Technologies Ltd.

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

#ifndef GCC_RISCV_VECTOR_BUILTINS_SHAPES_H
#define GCC_RISCV_VECTOR_BUILTINS_SHAPES_H

namespace riscv_vector {

namespace shapes {
extern const function_shape *const vsetvl;
extern const function_shape *const vsetvlmax;
extern const function_shape *const loadstore;
extern const function_shape *const indexed_loadstore;
extern const function_shape *const alu;
extern const function_shape *const widen_alu;
extern const function_shape *const no_mask_policy;
extern const function_shape *const return_mask;
extern const function_shape *const narrow_alu;
extern const function_shape *const move;
extern const function_shape *const mask_alu;
extern const function_shape *const reduc_alu;
extern const function_shape *const scalar_move;
extern const function_shape *const vundefined;
extern const function_shape *const misc;
extern const function_shape *const vset;
extern const function_shape *const vget;
extern const function_shape *const read_vl;
extern const function_shape *const fault_load;
extern const function_shape *const vlenb;
}

} // end namespace riscv_vector

#endif
