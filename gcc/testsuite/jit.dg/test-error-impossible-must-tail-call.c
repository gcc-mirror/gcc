#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

#ifdef __cplusplus
extern "C" {
#endif

  struct box { char dummy[64]; int i; };

  extern struct box
  returns_struct (int i);

#ifdef __cplusplus
}
#endif

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

       int test (int i)
       {
         return [MUST TAIL CALL] returns_struct (i).i;
       }

     and verify that we get a sane error when the tail call
     optimization can't be done.  */

  gcc_jit_type *char_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_CHAR);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  /* Declare "struct box.  */
  gcc_jit_type *array_type =
    gcc_jit_context_new_array_type (ctxt, NULL, char_type, 64);
  gcc_jit_field *field_dummy =
    gcc_jit_context_new_field (ctxt, NULL, array_type, "dummy");
  gcc_jit_field *field_i =
    gcc_jit_context_new_field (ctxt, NULL, int_type, "i");
  gcc_jit_field *fields[2] = {field_dummy, field_i};
  gcc_jit_struct *struct_box =
    gcc_jit_context_new_struct_type (ctxt, NULL, "box", 2, fields);

  /* Declare the imported function.  */
  gcc_jit_param *called_fn_param_i =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "i");
  gcc_jit_function *called_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_IMPORTED,
                                  gcc_jit_struct_as_type (struct_box),
                                  "called_function",
                                  1, &called_fn_param_i,
                                  0);

  /* Build the test_fn.  */
  gcc_jit_param *caller_param_i =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "i");
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  int_type,
                                  "test_caller",
                                  1, &caller_param_i,
                                  0);
  gcc_jit_rvalue *arg = gcc_jit_param_as_rvalue (caller_param_i);

  gcc_jit_rvalue *call =
    gcc_jit_context_new_call (ctxt, NULL,
                              called_fn,
                              1, &arg);

  /* Mark the call as requiring tail-call optimization.  */
  gcc_jit_rvalue_set_bool_require_tail_call (call, 1);

  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);
  gcc_jit_block_end_with_return (block, NULL,
    gcc_jit_rvalue_access_field (call, NULL, field_i));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "cannot tail-call: callee returns a structure");
}
