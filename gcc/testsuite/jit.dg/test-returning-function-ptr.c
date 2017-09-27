#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

#ifdef __cplusplus
extern "C" {
#endif

  extern void
  internally_called_function (int i, int j, int k);

#ifdef __cplusplus
}
#endif

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
     extern void internally_called_function (int i, int j, int k);

     static void
     internal_test_caller (int a)
     {
        internally_called_function (a * 3, a * 4, a * 5);
     }

     void (*) (int)
     get_test_caller (void)
     {
       return internal_test_caller;
     }
  */
  int i;
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  /* Declare the imported function.  */
  gcc_jit_param *params[3];
  params[0] =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "i");
  params[1] =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "j");
  params[2] =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "k");
  gcc_jit_function *called_fn =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_IMPORTED,
				  void_type,
				  "internally_called_function",
				  3, params,
				  0);

  /* Build the test_caller fn.  */
  gcc_jit_param *param_a =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "a");
  gcc_jit_function *test_caller =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  void_type,
				  "internal_test_caller",
				  1, &param_a,
				  0);
  /* "a * 3, a * 4, a * 5"  */
  gcc_jit_rvalue *args[3];
  for (i = 0; i < 3; i++)
    args[i]
      = gcc_jit_context_new_binary_op
	  (ctxt, NULL,
	   GCC_JIT_BINARY_OP_MULT,
	   int_type,
	   gcc_jit_param_as_rvalue (param_a),
	   gcc_jit_context_new_rvalue_from_int (ctxt,
						int_type,
						(i + 3) ));
  gcc_jit_block *block = gcc_jit_function_new_block (test_caller, NULL);
  gcc_jit_block_add_eval (
    block, NULL,
    gcc_jit_context_new_call (ctxt,
			      NULL,
			      called_fn,
			      3, args));
  gcc_jit_block_end_with_void_return (block, NULL);

  gcc_jit_rvalue *fn_ptr
    = gcc_jit_function_get_address (test_caller, NULL);

  gcc_jit_type *fn_ptr_type
    = gcc_jit_rvalue_get_type (fn_ptr);

  /* Build the get_test_caller fn.  */
  gcc_jit_function *get_test_caller =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  fn_ptr_type,
				  "get_test_caller",
				  0, NULL,
				  0);
  block = gcc_jit_function_new_block (get_test_caller, NULL);
  gcc_jit_block_end_with_return (block, NULL, fn_ptr);
}

static int called_with[3];

extern void
internally_called_function (int i, int j, int k)
{
  called_with[0] = i;
  called_with[1] = j;
  called_with[2] = k;
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef void (*test_caller_type) (int);
  typedef test_caller_type (*get_test_caller_type) (void);
  CHECK_NON_NULL (result);

  get_test_caller_type get_test_caller =
    (get_test_caller_type)gcc_jit_result_get_code (result, "get_test_caller");
  CHECK_NON_NULL (get_test_caller);

  test_caller_type test_caller = (test_caller_type)get_test_caller ();
  CHECK_NON_NULL (test_caller);

  called_with[0] = 0;
  called_with[1] = 0;
  called_with[2] = 0;

  /* Call the JIT-generated function.  */
  test_caller (5);

  /* Verify that it correctly called "internally_called_function".  */
  CHECK_VALUE (called_with[0], 15);
  CHECK_VALUE (called_with[1], 20);
  CHECK_VALUE (called_with[2], 25);
}
