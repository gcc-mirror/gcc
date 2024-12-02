/* function_base declaration for SiFive custom 'V' Extension for GNU compiler.
   Copyright (C) 2024 Free Software Foundation, Inc.
   Contributed by SiFive and PLCT Lab.

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

#ifndef GCC_SIFIVE_VECTOR_BUILTINS_BASES_H
#define GCC_SIFIVE_VECTOR_BUILTINS_BASES_H

namespace riscv_vector {

namespace bases {
extern const function_base *const sf_vqmacc;
extern const function_base *const sf_vqmaccu;
extern const function_base *const sf_vqmaccsu;
extern const function_base *const sf_vqmaccus;
extern const function_base *const sf_vfnrclip_x_f_qf;
extern const function_base *const sf_vfnrclip_xu_f_qf;
}

} // end namespace riscv_vector

#endif
