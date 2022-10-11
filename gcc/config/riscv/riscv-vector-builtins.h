/* Builtins definitions for RISC-V 'V' Extension for GNU compiler.
   Copyright (C) 2022-2022 Free Software Foundation, Inc.
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

#ifndef GCC_RISCV_VECTOR_BUILTINS_H
#define GCC_RISCV_VECTOR_BUILTINS_H

namespace riscv_vector {

/* This is for segment instructions.  */
const unsigned int MAX_TUPLE_SIZE = 8;

/* Enumerates the RVV types, together called
   "vector types" for brevity.  */
enum vector_type_index
{
#define DEF_RVV_TYPE(NAME, ABI_NAME, NCHARS, ARGS...)    \
  VECTOR_TYPE_##NAME,
#include "riscv-vector-builtins.def"
  NUM_VECTOR_TYPES
};

/* Builtin types that are used to register RVV intrinsics.  */
struct GTY (()) rvv_builtin_types_t
{
  tree vector;
  tree scalar;
  tree vector_ptr;
  tree scalar_ptr;
  tree scalar_const_ptr;
};

} // end namespace riscv_vector

#endif
