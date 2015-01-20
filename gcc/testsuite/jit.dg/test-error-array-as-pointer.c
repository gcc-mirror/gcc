#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>

#include "libgccjit.h"

#include "harness.h"

#define BUFFER_SIZE (1024)

char test_buffer[1024];

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
        void test_of_array_as_pointer (const char *name)
        {
            snprintf (test_buffer, sizeof (test_buffer),
	              "hello %s", name);
        }
    */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *const_char_ptr_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_CONST_CHAR_PTR);
  gcc_jit_type *char_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_CHAR);
  gcc_jit_type *char_ptr_type =
    gcc_jit_type_get_pointer (char_type);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *size_t_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_SIZE_T);
  gcc_jit_type *buf_type =
    gcc_jit_context_new_array_type (ctxt, NULL, char_type, BUFFER_SIZE);

  /* extern int snprintf(char *str, size_t size, const char *format, ...); */
  gcc_jit_param *param_s =
    gcc_jit_context_new_param (ctxt, NULL, char_ptr_type, "s");
  gcc_jit_param *param_n =
    gcc_jit_context_new_param (ctxt, NULL, size_t_type, "n");
  gcc_jit_param *param_format =
    gcc_jit_context_new_param (ctxt, NULL, const_char_ptr_type, "format");
  gcc_jit_param *snprintf_params[3] = {param_s, param_n, param_format};
  gcc_jit_function *snprintf =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_IMPORTED,
				  int_type,
				  "snprintf",
				  3, snprintf_params,
				  1);

  gcc_jit_param *param_name =
    gcc_jit_context_new_param (ctxt, NULL, const_char_ptr_type, "name");
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  void_type,
				  "test_of_array_as_pointer",
				  1, &param_name,
				  0);

  gcc_jit_lvalue *buffer =
    gcc_jit_context_new_global (ctxt, NULL,
				GCC_JIT_GLOBAL_IMPORTED,
				buf_type,
				"test_buffer");

  gcc_jit_block *block = gcc_jit_function_new_block(test_fn, "entry");

  /* snprintf(buffer, sizeof(buffer), "hello %s", name); */
  gcc_jit_rvalue *args[4];
  args[0] = gcc_jit_context_new_cast (
    ctxt, NULL,
    /* Here's the difference with test-array-as-pointer.c: */
    gcc_jit_lvalue_as_rvalue (buffer),
    char_ptr_type);
  args[1] = gcc_jit_context_new_rvalue_from_int (ctxt,
						 size_t_type,
						 BUFFER_SIZE);
  args[2] = gcc_jit_context_new_string_literal (ctxt, "hello %s");
  args[3] = gcc_jit_param_as_rvalue (param_name);

  gcc_jit_block_add_eval (
    block, NULL,
    gcc_jit_context_new_call (ctxt, NULL, snprintf, 4, args));
  gcc_jit_block_end_with_void_return (block, NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_context_new_cast:"
		      " cannot cast test_buffer"
		      " from type: char[1024]"
		      " to type: char *");
}
