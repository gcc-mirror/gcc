/* ACLE support for Arm MVE
   Copyright (C) 2021-2023 Free Software Foundation, Inc.

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

#ifndef GCC_ARM_MVE_BUILTINS_H
#define GCC_ARM_MVE_BUILTINS_H

namespace arm_mve {

/* Enumerates the MVE predicate and (data) vector types, together called
   "vector types" for brevity.  */
enum vector_type_index
{
#define DEF_MVE_TYPE(ACLE_NAME, SCALAR_TYPE) \
  VECTOR_TYPE_ ## ACLE_NAME,
#include "arm-mve-builtins.def"
  NUM_VECTOR_TYPES
#undef DEF_MVE_TYPE
};

extern tree scalar_types[NUM_VECTOR_TYPES];
extern tree acle_vector_types[3][NUM_VECTOR_TYPES + 1];

} /* end namespace arm_mve */

#endif /* GCC_ARM_MVE_BUILTINS_H */
