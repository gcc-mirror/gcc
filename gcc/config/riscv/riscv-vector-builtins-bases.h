/* function_base declaration for RISC-V 'V' Extension for GNU compiler.
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

#ifndef GCC_RISCV_VECTOR_BUILTINS_BASES_H
#define GCC_RISCV_VECTOR_BUILTINS_BASES_H

namespace riscv_vector {

namespace bases {
extern const function_base *const vsetvl;
extern const function_base *const vsetvlmax;
extern const function_base *const vle;
extern const function_base *const vse;
extern const function_base *const vlm;
extern const function_base *const vsm;
extern const function_base *const vlse;
extern const function_base *const vsse;
extern const function_base *const vluxei8;
extern const function_base *const vluxei16;
extern const function_base *const vluxei32;
extern const function_base *const vluxei64;
extern const function_base *const vloxei8;
extern const function_base *const vloxei16;
extern const function_base *const vloxei32;
extern const function_base *const vloxei64;
extern const function_base *const vsuxei8;
extern const function_base *const vsuxei16;
extern const function_base *const vsuxei32;
extern const function_base *const vsuxei64;
extern const function_base *const vsoxei8;
extern const function_base *const vsoxei16;
extern const function_base *const vsoxei32;
extern const function_base *const vsoxei64;
}

} // end namespace riscv_vector

#endif
