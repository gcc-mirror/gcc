#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libgccjit.h"

#include "harness.h"

typedef int v4si __attribute__ ((vector_size (16)));
typedef unsigned int v4ui __attribute__ ((vector_size (16)));
typedef float v4f __attribute__ ((vector_size (16)));

static void
create_vec_fn (gcc_jit_context *ctxt, const char *fnname,
	       gcc_jit_type *the_type, enum gcc_jit_binary_op op)
{
  /* Create equivalent to:

       static void
       FNNAME (const T *a, const T *b, T *c)
       {
         *c = *a OP *b;
       }

     where T is "the_type" (e.g. v4si).  */

  gcc_jit_type *ptr_type = gcc_jit_type_get_pointer (the_type);

  gcc_jit_type *const_type = gcc_jit_type_get_const (the_type);
  gcc_jit_type *ptr_to_const_type = gcc_jit_type_get_pointer (const_type);

  gcc_jit_param *a =
    gcc_jit_context_new_param (ctxt, NULL, ptr_to_const_type, "a");
  gcc_jit_param *b =
    gcc_jit_context_new_param (ctxt, NULL, ptr_to_const_type, "b");
  gcc_jit_param *c =
    gcc_jit_context_new_param (ctxt, NULL, ptr_type, "c");

  gcc_jit_type *return_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);

  gcc_jit_param *params[3] = {a, b, c};
  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  return_type,
				  fnname,
				  3, params, 0);
  gcc_jit_block *initial =
    gcc_jit_function_new_block (func, "initial");

  /* (*a OP *b) */
  gcc_jit_rvalue *op_result =
    gcc_jit_context_new_binary_op (
      ctxt, NULL,
      op,
      the_type,
      gcc_jit_lvalue_as_rvalue (gcc_jit_rvalue_dereference (gcc_jit_param_as_rvalue (a),
							    NULL)),
      gcc_jit_lvalue_as_rvalue (gcc_jit_rvalue_dereference (gcc_jit_param_as_rvalue (b),
							    NULL)));
  /* *c = *a OP *b; */
  gcc_jit_block_add_assignment (
    initial, NULL,
    gcc_jit_rvalue_dereference (gcc_jit_param_as_rvalue (c), NULL),
    op_result);
  gcc_jit_block_end_with_void_return (initial, NULL);
}

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_type *int_type = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *unsigned_type
    = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_UNSIGNED_INT);
  gcc_jit_type *float_type = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_FLOAT);

  gcc_jit_type *v4si_type = gcc_jit_type_get_vector (int_type, 4);
  gcc_jit_type *v4ui_type = gcc_jit_type_get_vector (unsigned_type, 4);
  gcc_jit_type *v4f_type = gcc_jit_type_get_vector (float_type, 4);

  create_vec_fn (ctxt, "jit_v4si_add",
		 v4si_type, GCC_JIT_BINARY_OP_PLUS);
  create_vec_fn (ctxt, "jit_v4si_sub",
		 v4si_type, GCC_JIT_BINARY_OP_MINUS);
  create_vec_fn (ctxt, "jit_v4si_mult",
		 v4si_type, GCC_JIT_BINARY_OP_MULT);
  create_vec_fn (ctxt, "jit_v4si_div",
		 v4si_type, GCC_JIT_BINARY_OP_DIVIDE);

  create_vec_fn (ctxt, "jit_v4ui_add",
		 v4ui_type, GCC_JIT_BINARY_OP_PLUS);
  create_vec_fn (ctxt, "jit_v4ui_sub",
		 v4ui_type, GCC_JIT_BINARY_OP_MINUS);
  create_vec_fn (ctxt, "jit_v4ui_mult",
		 v4ui_type, GCC_JIT_BINARY_OP_MULT);
  create_vec_fn (ctxt, "jit_v4ui_div",
		 v4ui_type, GCC_JIT_BINARY_OP_DIVIDE);

  create_vec_fn (ctxt, "jit_v4f_add",
		 v4f_type, GCC_JIT_BINARY_OP_PLUS);
  create_vec_fn (ctxt, "jit_v4f_sub",
		 v4f_type, GCC_JIT_BINARY_OP_MINUS);
  create_vec_fn (ctxt, "jit_v4f_mult",
		 v4f_type, GCC_JIT_BINARY_OP_MULT);
  create_vec_fn (ctxt, "jit_v4f_div",
		 v4f_type, GCC_JIT_BINARY_OP_DIVIDE);

  // Checking compatibility between types.
  CHECK_VALUE(gcc_jit_compatible_types(v4si_type, v4ui_type), 0);
  CHECK_VALUE(gcc_jit_compatible_types(v4si_type, v4f_type), 0);
  CHECK_VALUE(gcc_jit_compatible_types(v4ui_type, v4f_type), 0);

  gcc_jit_type *v4si_type2 = gcc_jit_type_get_vector (int_type, 4);
  gcc_jit_type *v4ui_type2 = gcc_jit_type_get_vector (unsigned_type, 4);
  gcc_jit_type *v4f_type2 = gcc_jit_type_get_vector (float_type, 4);

  CHECK_VALUE(gcc_jit_compatible_types(v4si_type, v4si_type2), 1);
  CHECK_VALUE(gcc_jit_compatible_types(v4ui_type, v4ui_type2), 1);
  CHECK_VALUE(gcc_jit_compatible_types(v4f_type, v4f_type2), 1);
}

template <typename T>
void
check_add (const T &a, const T &b, const T &c)
{
  for (int i = 0; i < 4; i++)
    CHECK_VALUE (c[i], a[i] + b[i]);
}

template <typename T>
void
check_sub (const T &a, const T &b, const T &c)
{
  for (int i = 0; i < 4; i++)
    CHECK_VALUE (c[i], a[i] - b[i]);
}

template <typename T>
void
check_mult (const T &a, const T &b, const T &c)
{
  for (int i = 0; i < 4; i++)
    CHECK_VALUE (c[i], a[i] * b[i]);
}

template <typename T>
void
check_div (const T &a, const T &b, const T &c)
{
  for (int i = 0; i < 4; i++)
    CHECK_VALUE (c[i], a[i] / b[i]);
}

template <>
void
check_div<v4f> (const v4f &a, const v4f &b, const v4f &c)
{
  for (int i = 0; i < 4; i++)
    CHECK_DOUBLE_VALUE (c[i], a[i] / b[i]);
}

template <typename T>
void
verify_vec_code (gcc_jit_context *ctxt, gcc_jit_result *result,
		 const char *fnname,
		 void (*check_cb) (const T &a, const T &b, const T &c))
{
  typedef void (*binop_type) (const T *a, const T *b, T *c);
  CHECK_NON_NULL (result);
  binop_type fn =
    (binop_type)gcc_jit_result_get_code (result, fnname);
  CHECK_NON_NULL (fn);

  T a, b, c;

  /* Init.  */
  for (int i = 0; i < 4; i++)
    {
      a[i] = i + 5;
      b[i] = (i + 4) * 3;
    }

  /* Run jit-compiled code and verify result.  */
  fn (&a, &b, &c);
  check_cb (a, b, c);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  verify_vec_code<v4si> (ctxt, result, "jit_v4si_add", check_add);
  verify_vec_code<v4si> (ctxt, result, "jit_v4si_sub", check_sub);
  verify_vec_code<v4si> (ctxt, result, "jit_v4si_mult", check_mult);
  verify_vec_code<v4si> (ctxt, result, "jit_v4si_div", check_div);

  verify_vec_code<v4ui> (ctxt, result, "jit_v4ui_add", check_add);
  verify_vec_code<v4ui> (ctxt, result, "jit_v4ui_sub", check_sub);
  verify_vec_code<v4ui> (ctxt, result, "jit_v4ui_mult", check_mult);
  verify_vec_code<v4ui> (ctxt, result, "jit_v4ui_div", check_div);

  verify_vec_code<v4f> (ctxt, result, "jit_v4f_add", check_add);
  verify_vec_code<v4f> (ctxt, result, "jit_v4f_sub", check_sub);
  verify_vec_code<v4f> (ctxt, result, "jit_v4f_mult", check_mult);
  verify_vec_code<v4f> (ctxt, result, "jit_v4f_div", check_div);
}
