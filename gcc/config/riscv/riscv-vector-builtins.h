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

/* RAII class for enabling enough RVV features to define the built-in
   types and implement the riscv_vector.h pragma.

   Note: According to 'TYPE_MODE' macro implementation, we need set
   have_regs_of_mode[mode] to be true if we want to get the exact mode
   from 'TYPE_MODE'. However, have_regs_of_mode has not been set yet in
   targetm.init_builtins (). We need rvv_switcher to set have_regs_of_mode
   before targetm.init_builtins () and recover back have_regs_of_mode
   after targetm.init_builtins ().  */
class rvv_switcher
{
public:
  rvv_switcher ();
  ~rvv_switcher ();

private:
  bool m_old_have_regs_of_mode[MAX_MACHINE_MODE];
};

} // end namespace riscv_vector

#endif
