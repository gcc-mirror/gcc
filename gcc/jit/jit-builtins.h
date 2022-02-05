/* jit-builtins.h -- Handling of builtin functions during JIT-compilation.
   Copyright (C) 2014-2022 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef JIT_BUILTINS_H
#define JIT_BUILTINS_H

#include "jit-common.h"

/* The following file contains several enumerations and data structures
   built from the definitions in i386-builtin-types.def.  */

//#include "config/i386/i386-builtins.h"

#define MULTI_ARG_4_DF2_DI_I	V2DF_FTYPE_V2DF_V2DF_V2DI_INT
#define MULTI_ARG_4_DF2_DI_I1	V4DF_FTYPE_V4DF_V4DF_V4DI_INT
#define MULTI_ARG_4_SF2_SI_I	V4SF_FTYPE_V4SF_V4SF_V4SI_INT
#define MULTI_ARG_4_SF2_SI_I1	V8SF_FTYPE_V8SF_V8SF_V8SI_INT
#define MULTI_ARG_3_SF		V4SF_FTYPE_V4SF_V4SF_V4SF
#define MULTI_ARG_3_DF		V2DF_FTYPE_V2DF_V2DF_V2DF
#define MULTI_ARG_3_SF2		V8SF_FTYPE_V8SF_V8SF_V8SF
#define MULTI_ARG_3_DF2		V4DF_FTYPE_V4DF_V4DF_V4DF
#define MULTI_ARG_3_DI		V2DI_FTYPE_V2DI_V2DI_V2DI
#define MULTI_ARG_3_SI		V4SI_FTYPE_V4SI_V4SI_V4SI
#define MULTI_ARG_3_SI_DI	V4SI_FTYPE_V4SI_V4SI_V2DI
#define MULTI_ARG_3_HI		V8HI_FTYPE_V8HI_V8HI_V8HI
#define MULTI_ARG_3_HI_SI	V8HI_FTYPE_V8HI_V8HI_V4SI
#define MULTI_ARG_3_QI		V16QI_FTYPE_V16QI_V16QI_V16QI
#define MULTI_ARG_3_DI2		V4DI_FTYPE_V4DI_V4DI_V4DI
#define MULTI_ARG_3_SI2		V8SI_FTYPE_V8SI_V8SI_V8SI
#define MULTI_ARG_3_HI2		V16HI_FTYPE_V16HI_V16HI_V16HI
#define MULTI_ARG_3_QI2		V32QI_FTYPE_V32QI_V32QI_V32QI
#define MULTI_ARG_2_SF		V4SF_FTYPE_V4SF_V4SF
#define MULTI_ARG_2_DF		V2DF_FTYPE_V2DF_V2DF
#define MULTI_ARG_2_DI		V2DI_FTYPE_V2DI_V2DI
#define MULTI_ARG_2_SI		V4SI_FTYPE_V4SI_V4SI
#define MULTI_ARG_2_HI		V8HI_FTYPE_V8HI_V8HI
#define MULTI_ARG_2_QI		V16QI_FTYPE_V16QI_V16QI
#define MULTI_ARG_2_DI_IMM	V2DI_FTYPE_V2DI_SI
#define MULTI_ARG_2_SI_IMM	V4SI_FTYPE_V4SI_SI
#define MULTI_ARG_2_HI_IMM	V8HI_FTYPE_V8HI_SI
#define MULTI_ARG_2_QI_IMM	V16QI_FTYPE_V16QI_SI
#define MULTI_ARG_2_DI_CMP	V2DI_FTYPE_V2DI_V2DI_CMP
#define MULTI_ARG_2_SI_CMP	V4SI_FTYPE_V4SI_V4SI_CMP
#define MULTI_ARG_2_HI_CMP	V8HI_FTYPE_V8HI_V8HI_CMP
#define MULTI_ARG_2_QI_CMP	V16QI_FTYPE_V16QI_V16QI_CMP
#define MULTI_ARG_2_SF_TF	V4SF_FTYPE_V4SF_V4SF_TF
#define MULTI_ARG_2_DF_TF	V2DF_FTYPE_V2DF_V2DF_TF
#define MULTI_ARG_2_DI_TF	V2DI_FTYPE_V2DI_V2DI_TF
#define MULTI_ARG_2_SI_TF	V4SI_FTYPE_V4SI_V4SI_TF
#define MULTI_ARG_2_HI_TF	V8HI_FTYPE_V8HI_V8HI_TF
#define MULTI_ARG_2_QI_TF	V16QI_FTYPE_V16QI_V16QI_TF
#define MULTI_ARG_1_SF		V4SF_FTYPE_V4SF
#define MULTI_ARG_1_DF		V2DF_FTYPE_V2DF
#define MULTI_ARG_1_SF2		V8SF_FTYPE_V8SF
#define MULTI_ARG_1_DF2		V4DF_FTYPE_V4DF
#define MULTI_ARG_1_DI		V2DI_FTYPE_V2DI
#define MULTI_ARG_1_SI		V4SI_FTYPE_V4SI
#define MULTI_ARG_1_HI		V8HI_FTYPE_V8HI
#define MULTI_ARG_1_QI		V16QI_FTYPE_V16QI
#define MULTI_ARG_1_SI_DI	V2DI_FTYPE_V4SI
#define MULTI_ARG_1_HI_DI	V2DI_FTYPE_V8HI
#define MULTI_ARG_1_HI_SI	V4SI_FTYPE_V8HI
#define MULTI_ARG_1_QI_DI	V2DI_FTYPE_V16QI
#define MULTI_ARG_1_QI_SI	V4SI_FTYPE_V16QI
#define MULTI_ARG_1_QI_HI	V8HI_FTYPE_V16QI


#include "i386-builtin-types.inc"

namespace gcc {

namespace jit {

/* Create an enum of the builtin types.  */

enum jit_builtin_type
{
#define DEF_PRIMITIVE_TYPE(NAME, VALUE) NAME,
#define DEF_FUNCTION_TYPE_0(NAME, RETURN) NAME,
#define DEF_FUNCTION_TYPE_1(NAME, RETURN, ARG1) NAME,
#define DEF_FUNCTION_TYPE_2(NAME, RETURN, ARG1, ARG2) NAME,
#define DEF_FUNCTION_TYPE_3(NAME, RETURN, ARG1, ARG2, ARG3) NAME,
#define DEF_FUNCTION_TYPE_4(NAME, RETURN, ARG1, ARG2, ARG3, ARG4) NAME,
#define DEF_FUNCTION_TYPE_5(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5) NAME,
#define DEF_FUNCTION_TYPE_6(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6) NAME,
#define DEF_FUNCTION_TYPE_7(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7) NAME,
#define DEF_FUNCTION_TYPE_8(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8) NAME,
#define DEF_FUNCTION_TYPE_9(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8, ARG9) NAME,
#define DEF_FUNCTION_TYPE_10(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			     ARG6, ARG7, ARG8, ARG9, ARG10) NAME,
#define DEF_FUNCTION_TYPE_11(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			     ARG6, ARG7, ARG8, ARG9, ARG10, ARG11) NAME,
#define DEF_FUNCTION_TYPE_VAR_0(NAME, RETURN) NAME,
#define DEF_FUNCTION_TYPE_VAR_1(NAME, RETURN, ARG1) NAME,
#define DEF_FUNCTION_TYPE_VAR_2(NAME, RETURN, ARG1, ARG2) NAME,
#define DEF_FUNCTION_TYPE_VAR_3(NAME, RETURN, ARG1, ARG2, ARG3) NAME,
#define DEF_FUNCTION_TYPE_VAR_4(NAME, RETURN, ARG1, ARG2, ARG3, ARG4) NAME,
#define DEF_FUNCTION_TYPE_VAR_5(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5) \
				NAME,
#define DEF_FUNCTION_TYPE_VAR_6(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6) NAME,
#define DEF_FUNCTION_TYPE_VAR_7(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6, ARG7) NAME,
#define DEF_POINTER_TYPE(NAME, TYPE) NAME,
#include "builtin-types.def"
#undef DEF_PRIMITIVE_TYPE
#undef DEF_FUNCTION_TYPE_0
#undef DEF_FUNCTION_TYPE_1
#undef DEF_FUNCTION_TYPE_2
#undef DEF_FUNCTION_TYPE_3
#undef DEF_FUNCTION_TYPE_4
#undef DEF_FUNCTION_TYPE_5
#undef DEF_FUNCTION_TYPE_6
#undef DEF_FUNCTION_TYPE_7
#undef DEF_FUNCTION_TYPE_8
#undef DEF_FUNCTION_TYPE_9
#undef DEF_FUNCTION_TYPE_10
#undef DEF_FUNCTION_TYPE_11
#undef DEF_FUNCTION_TYPE_VAR_0
#undef DEF_FUNCTION_TYPE_VAR_1
#undef DEF_FUNCTION_TYPE_VAR_2
#undef DEF_FUNCTION_TYPE_VAR_3
#undef DEF_FUNCTION_TYPE_VAR_4
#undef DEF_FUNCTION_TYPE_VAR_5
#undef DEF_FUNCTION_TYPE_VAR_6
#undef DEF_FUNCTION_TYPE_VAR_7
#undef DEF_POINTER_TYPE
  BT_LAST
}; /* enum jit_builtin_type */


/* Create an enum of the target builtins.  */

enum jit_target_builtin
{
#define BDESC_FIRST(arg1, arg2, mask, arg4, arg5, name, code, arg8, func_type) code,
#define BDESC(mask, arg2, arg3, name, code, arg6, func_type) code,
#define BDESC_END(PCMPESTR, PCMPISTR)
#include "config/i386/i386-builtin.def"
#undef BDESC_FIRST
#undef BDESC
#undef BDESC_END
  TBT_LAST
}; /* enum jit_target_builtin */

struct jit_target_builtin_data
{
  const char* name;
  jit_target_builtin code;
  int /*ix86_builtin_func_type*/ type;
};

/* Create an array of the target builtins.  */

static struct jit_target_builtin_data target_builtin_data[] =
{
#define BDESC_FIRST(arg1, arg2, mask, arg4, arg5, name, code, arg8, func_type) { name, code, func_type },
#define BDESC(mask, arg2, arg3, name, code, arg6, func_type) { name, code, func_type },
#define BDESC_END(PCMPESTR, PCMPISTR)
#include "config/i386/i386-builtin.def"
#undef BDESC_FIRST
#undef BDESC
#undef BDESC_END
};

/* Create an enum of the attributes that can be present on builtins.  */

enum built_in_attribute
{
#define DEF_ATTR_NULL_TREE(ENUM) ENUM,
#define DEF_ATTR_INT(ENUM, VALUE) ENUM,
#define DEF_ATTR_STRING(ENUM, VALUE) ENUM,
#define DEF_ATTR_IDENT(ENUM, STRING) ENUM,
#define DEF_ATTR_TREE_LIST(ENUM, PURPOSE, VALUE, CHAIN) ENUM,
#include "builtin-attrs.def"
#undef DEF_ATTR_NULL_TREE
#undef DEF_ATTR_INT
#undef DEF_ATTR_STRING
#undef DEF_ATTR_IDENT
#undef DEF_ATTR_TREE_LIST
  ATTR_LAST
};

/***********************************************************************/

class builtins_manager
{
public:
  builtins_manager (recording::context *ctxt);

  recording::function *
  get_builtin_function (const char *name);

  static enum built_in_class
  get_class (enum built_in_function builtin_id);

  static bool
  implicit_p (enum built_in_function builtin_id);

  tree
  get_attrs_tree (enum built_in_function builtin_id);

  tree
  get_attrs_tree (enum built_in_attribute attr);

  void
  ensure_optimization_builtins_exist ();

  void
  finish_playback (void);

private:
  recording::function *
  get_builtin_function_by_id (enum built_in_function builtin_id);

  recording::function *
  make_builtin_function (enum built_in_function builtin_id);

  recording::type *
  get_type (enum jit_builtin_type type_id);

  recording::type *
  make_type (enum jit_builtin_type type_id);

  recording::type*
  make_primitive_type (enum jit_builtin_type type_id);

  recording::function_type*
  make_fn_type (enum jit_builtin_type type_id,
		enum jit_builtin_type return_type_id,
		bool is_variadic,
		int num_args, ...);

  recording::type*
  make_ptr_type (enum jit_builtin_type type_id,
		 enum jit_builtin_type other_type_id);

  tree
  make_attrs_tree (enum built_in_attribute attr);

private:
  /* Recording fields.  */
  recording::context *m_ctxt;
  recording::type *m_types[BT_LAST];
  recording::function *m_builtin_functions[END_BUILTINS];

  /* Playback fields.  */
  /* m_attributes is not GTY-marked, but is only ever used from within
     the region of playback::context::replay () in which a GC can't
     happen.  */
  tree m_attributes[ATTR_LAST];
};

} // namespace jit
} // namespace gcc

#endif /* JIT_BUILTINS_H */
