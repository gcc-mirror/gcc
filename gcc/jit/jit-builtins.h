/* jit-builtins.h -- Handling of builtin functions during JIT-compilation.
   Copyright (C) 2014 Free Software Foundation, Inc.

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

namespace gcc {

namespace jit {

namespace recording {

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
#define DEF_FUNCTION_TYPE_6(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6) NAME,
#define DEF_FUNCTION_TYPE_7(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7) NAME,
#define DEF_FUNCTION_TYPE_8(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7, ARG8) NAME,
#define DEF_FUNCTION_TYPE_VAR_0(NAME, RETURN) NAME,
#define DEF_FUNCTION_TYPE_VAR_1(NAME, RETURN, ARG1) NAME,
#define DEF_FUNCTION_TYPE_VAR_2(NAME, RETURN, ARG1, ARG2) NAME,
#define DEF_FUNCTION_TYPE_VAR_3(NAME, RETURN, ARG1, ARG2, ARG3) NAME,
#define DEF_FUNCTION_TYPE_VAR_4(NAME, RETURN, ARG1, ARG2, ARG3, ARG4) NAME,
#define DEF_FUNCTION_TYPE_VAR_5(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG6) \
  NAME,
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
#undef DEF_FUNCTION_TYPE_VAR_0
#undef DEF_FUNCTION_TYPE_VAR_1
#undef DEF_FUNCTION_TYPE_VAR_2
#undef DEF_FUNCTION_TYPE_VAR_3
#undef DEF_FUNCTION_TYPE_VAR_4
#undef DEF_FUNCTION_TYPE_VAR_5
#undef DEF_POINTER_TYPE
  BT_LAST
}; /* enum jit_builtin_type */

/***********************************************************************/

class builtins_manager
{
public:
  builtins_manager (context *ctxt);

  function *
  get_builtin_function (const char *name);

private:
  function *make_builtin_function (enum built_in_function builtin_id);

  type *get_type (enum jit_builtin_type type_id);

  type *make_type (enum jit_builtin_type type_id);

  type*
  make_primitive_type (enum jit_builtin_type type_id);

  function_type*
  make_fn_type (enum jit_builtin_type type_id,
		enum jit_builtin_type return_type_id,
		bool is_variadic,
		int num_args, ...);

  type*
  make_ptr_type (enum jit_builtin_type type_id,
		 enum jit_builtin_type other_type_id);

private:
  context *m_ctxt;
  type *m_types[BT_LAST];
  function *m_builtin_functions[END_BUILTINS];
};

} // namespace recording
} // namespace jit
} // namespace gcc

#endif /* JIT_BUILTINS_H */
