/* Common data structures used for builtin handling on S/390.
   Copyright (C) 2015-2023 Free Software Foundation, Inc.

   Contributed by Andreas Krebbel (Andreas.Krebbel@de.ibm.com).

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* This files contains data structure definitions which can be used by
   s390-builtins.cc as well as s390-c.cc.  Since the latter is
   considered to be part of the front-end we have to be careful not
   to use any of tree and rtx like data structures.  */

/* Builtin types, data and prototypes. */

enum s390_builtin_type_index
{
#undef DEF_TYPE
#undef DEF_POINTER_TYPE
#undef DEF_DISTINCT_TYPE
#undef DEF_VECTOR_TYPE
#undef DEF_OPAQUE_VECTOR_TYPE
#undef DEF_FN_TYPE
#undef DEF_OV_TYPE
#define DEF_TYPE(INDEX, ...) INDEX,
#define DEF_POINTER_TYPE(INDEX, ...) INDEX,
#define DEF_DISTINCT_TYPE(INDEX, ...) INDEX,
#define DEF_VECTOR_TYPE(INDEX, ...) INDEX,
#define DEF_OPAQUE_VECTOR_TYPE(INDEX, ...) INDEX,
#define DEF_FN_TYPE(...)
#define DEF_OV_TYPE(...)
#include "s390-builtin-types.def"
  BT_MAX
};

enum s390_builtin_fn_type_index
{
#undef DEF_TYPE
#undef DEF_POINTER_TYPE
#undef DEF_DISTINCT_TYPE
#undef DEF_VECTOR_TYPE
#undef DEF_OPAQUE_VECTOR_TYPE
#undef DEF_FN_TYPE
#undef DEF_OV_TYPE
#define DEF_TYPE(...)
#define DEF_POINTER_TYPE(...)
#define DEF_DISTINCT_TYPE(...)
#define DEF_VECTOR_TYPE(...)
#define DEF_OPAQUE_VECTOR_TYPE(...)
#define DEF_FN_TYPE(INDEX, ...) INDEX,
#define DEF_OV_TYPE(...)
#include "s390-builtin-types.def"
  BT_FN_MAX
};

enum s390_builtin_ov_type_index
{
#undef DEF_TYPE
#undef DEF_POINTER_TYPE
#undef DEF_DISTINCT_TYPE
#undef DEF_VECTOR_TYPE
#undef DEF_OPAQUE_VECTOR_TYPE
#undef DEF_FN_TYPE
#undef DEF_OV_TYPE
#define DEF_TYPE(...)
#define DEF_POINTER_TYPE(...)
#define DEF_DISTINCT_TYPE(...)
#define DEF_VECTOR_TYPE(...)
#define DEF_OPAQUE_VECTOR_TYPE(...)
#define DEF_FN_TYPE(...)
#define DEF_OV_TYPE(INDEX, ...) INDEX,
#include "s390-builtin-types.def"
  BT_OV_MAX
};

#define MAX_OV_OPERANDS 6

extern tree s390_builtin_types[BT_MAX];
extern tree s390_builtin_fn_types[BT_FN_MAX];

  /* Builtins.  */

enum s390_builtins {
#undef B_DEF
#undef OB_DEF
#undef OB_DEF_VAR
#define B_DEF(NAME, ...) S390_BUILTIN_##NAME,
#define OB_DEF(...)
#define OB_DEF_VAR(...)

#include "s390-builtins.def"
  S390_BUILTIN_MAX
};


/* Generate an enumeration of all overloaded builtins defined with
   OB_DEF in s390-builtins.def.  */
enum s390_overloaded_builtins {
#undef B_DEF
#undef OB_DEF
#undef OB_DEF_VAR
#define B_DEF(...)
#define OB_DEF(NAME, ...) S390_OVERLOADED_BUILTIN_##NAME,
#define OB_DEF_VAR(...)
#include "s390-builtins.def"
S390_OVERLOADED_BUILTIN_MAX
};

/* Generate an enumeration of all variants of overloaded builtins
   defined with OB_DEF_VAR in s390-builtins.def.  */
enum s390_overloaded_builtin_vars {
#undef B_DEF
#undef OB_DEF
#undef OB_DEF_VAR
#define B_DEF(...)
#define OB_DEF(...)
#define OB_DEF_VAR(NAME, ...) S390_OVERLOADED_BUILTIN_VAR_##NAME,
#include "s390-builtins.def"
S390_OVERLOADED_BUILTIN_VAR_MAX
};

#define S390_OVERLOADED_BUILTIN_OFFSET S390_BUILTIN_MAX
#define S390_OVERLOADED_BUILTIN_VAR_OFFSET \
  (S390_BUILTIN_MAX + S390_OVERLOADED_BUILTIN_MAX)
#define S390_ALL_BUILTIN_MAX				\
  (S390_BUILTIN_MAX + S390_OVERLOADED_BUILTIN_MAX +	\
   S390_OVERLOADED_BUILTIN_VAR_MAX)

extern const unsigned int bflags_builtin[S390_BUILTIN_MAX + 1];
extern const unsigned int opflags_builtin[S390_BUILTIN_MAX + 1];

extern const unsigned int
  bflags_overloaded_builtin[S390_OVERLOADED_BUILTIN_MAX + 1];
extern const unsigned int
  bflags_overloaded_builtin_var[S390_OVERLOADED_BUILTIN_VAR_MAX + 1];
extern const unsigned int
  opflags_overloaded_builtin_var[S390_OVERLOADED_BUILTIN_VAR_MAX + 1];

static inline unsigned int
bflags_for_builtin (int fcode)
{
  if (fcode >= S390_OVERLOADED_BUILTIN_VAR_OFFSET)
    gcc_unreachable ();
  else if (fcode >= S390_OVERLOADED_BUILTIN_OFFSET)
    return bflags_overloaded_builtin[fcode - S390_BUILTIN_MAX];
  else
    return bflags_builtin[fcode];
}

static inline unsigned int
opflags_for_builtin (int fcode)
{
  if (fcode >= S390_OVERLOADED_BUILTIN_VAR_OFFSET)
    return opflags_overloaded_builtin_var[fcode -
					  S390_OVERLOADED_BUILTIN_VAR_OFFSET];
  else if (fcode >= S390_OVERLOADED_BUILTIN_OFFSET)
    gcc_unreachable ();
  else
    return opflags_builtin[fcode];
}

extern tree s390_builtin_decls[S390_BUILTIN_MAX +
			       S390_OVERLOADED_BUILTIN_MAX +
			       S390_OVERLOADED_BUILTIN_VAR_MAX];
