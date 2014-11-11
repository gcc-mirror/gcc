#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

struct foo
{
  int x;
  int y;
  int z;
};

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
     void
     test_access (struct foo *f)
     {
        f->z = f->x * f->y;
     }
  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_field *x =
    gcc_jit_context_new_field (ctxt,
                               NULL,
                               int_type,
                               "x");
  gcc_jit_field *y =
    gcc_jit_context_new_field (ctxt,
                               NULL,
                               int_type,
                               "y");
  gcc_jit_field *z =
    gcc_jit_context_new_field (ctxt,
                               NULL,
                               int_type,
                               "z");
  gcc_jit_field *fields[] = {x, y, z};
  gcc_jit_struct *struct_type =
    gcc_jit_context_new_struct_type (ctxt, NULL, "foo", 3, fields);
  gcc_jit_type *ptr_type =
    gcc_jit_type_get_pointer (gcc_jit_struct_as_type (struct_type));

  /* Build the test function.  */
  gcc_jit_param *param_f =
    gcc_jit_context_new_param (ctxt, NULL, ptr_type, "f");
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  void_type,
                                  "test_access",
                                  1, &param_f,
                                  0);

  /* f->x * f->y */
  gcc_jit_rvalue *sum =
    gcc_jit_context_new_binary_op (
      ctxt, NULL,
      GCC_JIT_BINARY_OP_MULT,
      int_type,
      gcc_jit_lvalue_as_rvalue (
	gcc_jit_rvalue_dereference_field (
	  gcc_jit_param_as_rvalue (param_f),
	  NULL,
	  x)),
      gcc_jit_lvalue_as_rvalue (
	gcc_jit_rvalue_dereference_field (
	gcc_jit_param_as_rvalue (param_f),
	NULL,
	y)));

  /* f->z = ... */
  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);
  gcc_jit_block_add_assignment (
    block,
    NULL,
    gcc_jit_rvalue_dereference_field (
      gcc_jit_param_as_rvalue (param_f),
      NULL,
      z),
    sum);
  gcc_jit_block_end_with_void_return (block, NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef void (*fn_type) (struct foo *);
  CHECK_NON_NULL (result);

  fn_type test_access =
    (fn_type)gcc_jit_result_get_code (result, "test_access");
  CHECK_NON_NULL (test_access);

  struct foo tmp;
  tmp.x = 5;
  tmp.y = 7;
  tmp.z = 0;

  /* Call the JIT-generated function.  */
  test_access (&tmp);

  /* Verify that the code correctly modified the field "z".  */
  CHECK_VALUE (tmp.z, 35);
}

