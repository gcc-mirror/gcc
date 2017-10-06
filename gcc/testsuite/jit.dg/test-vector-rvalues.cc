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
	       gcc_jit_type *vec_type,
	       gcc_jit_type *elem_type,
	       enum gcc_jit_binary_op op)
{
  /* Create equivalent to:

       static void
       FNNAME (V *dst, const V *lhs, E p, E q, E r, E s)
       {
	 V pqrs;
	 pqrs = {p, q, r, s};
	 *dst = *lhs OP pqrs;
       }

     where V is "vec_type" (e.g. v4si)
     and   E is "elem_type" (e.g. int).  */

  gcc_jit_type *ptr_type = gcc_jit_type_get_pointer (vec_type);

  gcc_jit_type *const_type = gcc_jit_type_get_const (vec_type);
  gcc_jit_type *ptr_to_const_type = gcc_jit_type_get_pointer (const_type);

  gcc_jit_param *dst =
    gcc_jit_context_new_param (ctxt, NULL, ptr_type, "dst");
  gcc_jit_param *lhs =
    gcc_jit_context_new_param (ctxt, NULL, ptr_to_const_type, "lhs");
  gcc_jit_param *p =
    gcc_jit_context_new_param (ctxt, NULL, elem_type, "p");
  gcc_jit_param *q =
    gcc_jit_context_new_param (ctxt, NULL, elem_type, "q");
  gcc_jit_param *r =
    gcc_jit_context_new_param (ctxt, NULL, elem_type, "r");
  gcc_jit_param *s =
    gcc_jit_context_new_param (ctxt, NULL, elem_type, "s");

  gcc_jit_type *return_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);

  gcc_jit_param *params[6] = {dst, lhs, p, q, r, s};
  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  return_type,
				  fnname,
				  6, params, 0);
  gcc_jit_block *initial =
    gcc_jit_function_new_block (func, "initial");

  /* V pqrs; */
  gcc_jit_lvalue *pqrs
    = gcc_jit_function_new_local (func, NULL,
				  vec_type, "pqrs");

  /* pqrs = {p, q, r, s}; */
  gcc_jit_rvalue *elems[4];
  elems[0] = gcc_jit_param_as_rvalue (p);
  elems[1] = gcc_jit_param_as_rvalue (q);
  elems[2] = gcc_jit_param_as_rvalue (r);
  elems[3] = gcc_jit_param_as_rvalue (s);
  gcc_jit_block_add_assignment (
    initial, NULL,
    pqrs,
    gcc_jit_context_new_rvalue_from_vector (ctxt, NULL, vec_type, 4, elems));

  /* (*lhs OP pqrs) */
  gcc_jit_rvalue *op_result =
    gcc_jit_context_new_binary_op (
      ctxt, NULL,
      op,
      vec_type,
      gcc_jit_lvalue_as_rvalue (gcc_jit_rvalue_dereference (gcc_jit_param_as_rvalue (lhs),
							    NULL)),
      gcc_jit_lvalue_as_rvalue (pqrs));
  /* *dst = *lhs OP pqrs; */
  gcc_jit_block_add_assignment (
    initial, NULL,
    gcc_jit_rvalue_dereference (gcc_jit_param_as_rvalue (dst), NULL),
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
		 v4si_type, int_type, GCC_JIT_BINARY_OP_PLUS);
  create_vec_fn (ctxt, "jit_v4si_sub",
		 v4si_type, int_type, GCC_JIT_BINARY_OP_MINUS);
  create_vec_fn (ctxt, "jit_v4si_mult",
		 v4si_type, int_type, GCC_JIT_BINARY_OP_MULT);
  create_vec_fn (ctxt, "jit_v4si_div",
		 v4si_type, int_type, GCC_JIT_BINARY_OP_DIVIDE);

  create_vec_fn (ctxt, "jit_v4ui_add",
		 v4ui_type, unsigned_type, GCC_JIT_BINARY_OP_PLUS);
  create_vec_fn (ctxt, "jit_v4ui_sub",
		 v4ui_type, unsigned_type, GCC_JIT_BINARY_OP_MINUS);
  create_vec_fn (ctxt, "jit_v4ui_mult",
		 v4ui_type, unsigned_type, GCC_JIT_BINARY_OP_MULT);
  create_vec_fn (ctxt, "jit_v4ui_div",
		 v4ui_type, unsigned_type, GCC_JIT_BINARY_OP_DIVIDE);

  create_vec_fn (ctxt, "jit_v4f_add",
		 v4f_type, float_type, GCC_JIT_BINARY_OP_PLUS);
  create_vec_fn (ctxt, "jit_v4f_sub",
		 v4f_type, float_type, GCC_JIT_BINARY_OP_MINUS);
  create_vec_fn (ctxt, "jit_v4f_mult",
		 v4f_type, float_type, GCC_JIT_BINARY_OP_MULT);
  create_vec_fn (ctxt, "jit_v4f_div",
		 v4f_type, float_type, GCC_JIT_BINARY_OP_DIVIDE);
}

template <typename V>
void
check_add (const V &a, const V &b, const V &c)
{
  for (int i = 0; i < 4; i++)
    CHECK_VALUE (c[i], a[i] + b[i]);
}

template <typename V>
void
check_sub (const V &a, const V &b, const V &c)
{
  for (int i = 0; i < 4; i++)
    CHECK_VALUE (c[i], a[i] - b[i]);
}

template <typename V>
void
check_mult (const V &a, const V &b, const V &c)
{
  for (int i = 0; i < 4; i++)
    CHECK_VALUE (c[i], a[i] * b[i]);
}

template <typename V>
void
check_div (const V &a, const V &b, const V &c)
{
  for (int i = 0; i < 4; i++)
    CHECK_VALUE (c[i], a[i] / b[i]);
}

template <typename V, typename E>
void
verify_vec_code (gcc_jit_context *ctxt, gcc_jit_result *result,
		 const char *fnname,
		 void (*check_cb) (const V &a, const V &b, const V &c))
{
  typedef void (*binop_type) (const V *a, V *b, E p, E q, E r, E s);
  CHECK_NON_NULL (result);
  binop_type fn =
    (binop_type)gcc_jit_result_get_code (result, fnname);
  CHECK_NON_NULL (fn);

  V dst, lhs, pqrs;

  /* Init.  */
  for (int i = 0; i < 4; i++)
    {
      lhs[i] = i + 5;
      pqrs[i] = (i + 4) * 3;
    }

  /* Run jit-compiled code and verify result.  */
  fn (&dst, &lhs, pqrs[0], pqrs[1], pqrs[2], pqrs[3]);
  check_cb (lhs, pqrs, dst);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  verify_vec_code<v4si, int> (ctxt, result, "jit_v4si_add", check_add);
  verify_vec_code<v4si, int> (ctxt, result, "jit_v4si_sub", check_sub);
  verify_vec_code<v4si, int> (ctxt, result, "jit_v4si_mult", check_mult);
  verify_vec_code<v4si, int> (ctxt, result, "jit_v4si_div", check_div);

  verify_vec_code<v4ui, unsigned int> (ctxt, result, "jit_v4ui_add", check_add);
  verify_vec_code<v4ui, unsigned int> (ctxt, result, "jit_v4ui_sub", check_sub);
  verify_vec_code<v4ui, unsigned int> (ctxt, result, "jit_v4ui_mult", check_mult);
  verify_vec_code<v4ui, unsigned int> (ctxt, result, "jit_v4ui_div", check_div);

  verify_vec_code<v4f, float> (ctxt, result, "jit_v4f_add", check_add);
  verify_vec_code<v4f, float> (ctxt, result, "jit_v4f_sub", check_sub);
  verify_vec_code<v4f, float> (ctxt, result, "jit_v4f_mult", check_mult);
  verify_vec_code<v4f, float> (ctxt, result, "jit_v4f_div", check_div);
}
