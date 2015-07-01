/* Test of PR jit/66700.  */

#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

#ifdef __cplusplus
extern "C" {
#endif

  extern void
  write_back_through_ptr (double *d);

#ifdef __cplusplus
}
#endif

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

     double
     test_caller_of_write_back_through_ptr (void)
     {
       double d;
       d = 4.0;
       write_back_through_ptr (&d);
       return d;
     }
  */
  gcc_jit_type *t_void =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *t_double =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_DOUBLE);
  gcc_jit_type *t_ptr_to_double =
    gcc_jit_type_get_pointer (t_double);

  /* Declare the imported function.  */
  gcc_jit_param *params[1];
  params[0] =
    gcc_jit_context_new_param (ctxt, NULL, t_ptr_to_double, "d");
  gcc_jit_function *called_fn =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_IMPORTED,
				  t_void,
				  "write_back_through_ptr",
				  1, params,
				  0);

  /* Build the test_fn.  */
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  t_double,
				  "test_caller_of_write_back_through_ptr",
				  0, NULL,
				  0);
  gcc_jit_lvalue *d =
    gcc_jit_function_new_local (test_fn, NULL, t_double, "d");

  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);

  /* "d = 0.0" */
  gcc_jit_block_add_assignment (
    block, NULL, d,
    gcc_jit_context_new_rvalue_from_int (ctxt, t_double, 4));

  /* "write_back_through_ptr (&d);" */
  gcc_jit_rvalue *args[1];
  args[0] = gcc_jit_lvalue_get_address (d, NULL);
  gcc_jit_block_add_eval (
    block, NULL,
    gcc_jit_context_new_call (ctxt,
			      NULL,
			      called_fn,
			      1, args));
  gcc_jit_block_end_with_return (block,
				 NULL,
				 gcc_jit_lvalue_as_rvalue (d));
}

extern void
write_back_through_ptr (double *d)
{
  *d = 5.600000;
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef double (*fn_type) (void);
  CHECK_NON_NULL (result);

  fn_type test_caller_of_write_back_through_ptr =
    (fn_type)gcc_jit_result_get_code (result,
				      "test_caller_of_write_back_through_ptr");
  CHECK_NON_NULL (test_caller_of_write_back_through_ptr);

  /* Call the JIT-generated function.  */
  double d = test_caller_of_write_back_through_ptr ();

  /* Verify that it correctly called "write_back_through_ptr".  */
  CHECK_VALUE (d, 5.600000);
}

