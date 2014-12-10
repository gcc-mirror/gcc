#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

struct foo
{
  int x;
  int y;
};

struct bar
{
  int p;
  int q;
};

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
     void
     test_bogus_access (struct foo *f)
     {
        f->p = f->q;
     }
     i.e. using the wrong struct.
  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  /* Map "struct foo".  */
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
  gcc_jit_field *foo_fields[] = {x, y};
  gcc_jit_struct *struct_foo =
    gcc_jit_context_new_struct_type (ctxt, NULL, "foo", 2, foo_fields);

  /* Map "struct bar".  */
  gcc_jit_field *p =
    gcc_jit_context_new_field (ctxt,
                               NULL,
                               int_type,
                               "p");
  gcc_jit_field *q =
    gcc_jit_context_new_field (ctxt,
                               NULL,
                               int_type,
                               "q");
  /* We don't actually need a gcc_jit_type for "struct bar" for the test.  */
  gcc_jit_field *bar_fields[] = {p, q};
  (void)gcc_jit_context_new_struct_type (ctxt, NULL, "foo", 2, bar_fields);

  gcc_jit_type *foo_ptr =
    gcc_jit_type_get_pointer (gcc_jit_struct_as_type (struct_foo));

  /* Build the test function.  */
  gcc_jit_param *param_f =
    gcc_jit_context_new_param (ctxt, NULL, foo_ptr, "f");
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  void_type,
                                  "test_bogus_access",
                                  1, &param_f,
                                  0);

  /* Erroneous: f->p = ... */
  gcc_jit_lvalue *lvalue =
    gcc_jit_rvalue_dereference_field (
      gcc_jit_param_as_rvalue (param_f),
      NULL,
      p);

  /* Erroneous: ... = f->q; */
  gcc_jit_rvalue *rvalue =
    gcc_jit_lvalue_as_rvalue (
      gcc_jit_rvalue_dereference_field (
	gcc_jit_param_as_rvalue (param_f),
	NULL,
	q));

  gcc_jit_block *block =
    gcc_jit_function_new_block (test_fn, NULL);
  gcc_jit_block_add_assignment (
    block,
    NULL,
    lvalue, rvalue);
  gcc_jit_block_end_with_void_return (block, NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_rvalue_dereference_field:"
		      " p is not a field of struct foo");
}

