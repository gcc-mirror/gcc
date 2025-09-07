/* function_base declaration for Andes custom 'V' Extension for GNU compiler.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.
   Contributed by Andes.

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

#ifndef GCC_ANDES_VECTOR_BUILTINS_BASES_H
#define GCC_ANDES_VECTOR_BUILTINS_BASES_H

namespace riscv_vector {

namespace bases {
extern const function_base *const nds_vfwcvt_s;
extern const function_base *const nds_vfncvt_bf16;
extern const function_base *const nds_vfncvt_bf16_frm;
extern const function_base *const nds_vln8;
extern const function_base *const nds_vlnu8;
extern const function_base *const nds_vfpmadt;
extern const function_base *const nds_vfpmadb;
extern const function_base *const nds_vfpmadt_frm;
extern const function_base *const nds_vfpmadb_frm;
extern const function_base *const nds_vd4dots;
extern const function_base *const nds_vd4dotu;
extern const function_base *const nds_vd4dotsu;
}

} // end namespace riscv_vector

#endif
