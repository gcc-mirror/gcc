#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#define TEST_COMPILING_TO_FILE
#define OUTPUT_KIND      GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY
#define OUTPUT_FILENAME  "output-of-test-compile-to-dynamic-library.c.so"
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
     void
     hello_world (const char *name)
     {
       // a test comment
       printf ("hello from %s\n", name);
     }
  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *const_char_ptr_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_CONST_CHAR_PTR);
  gcc_jit_param *param_name =
    gcc_jit_context_new_param (ctxt, NULL, const_char_ptr_type, "name");
  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  void_type,
				  "hello_world",
				  1, &param_name,
				  0);

  gcc_jit_param *param_format =
    gcc_jit_context_new_param (ctxt, NULL, const_char_ptr_type, "format");
  gcc_jit_function *printf_func =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_IMPORTED,
				  gcc_jit_context_get_type (
				     ctxt, GCC_JIT_TYPE_INT),
				  "printf",
				  1, &param_format,
				  1);
  gcc_jit_rvalue *args[2];
  args[0] = gcc_jit_context_new_string_literal (ctxt, "hello from %s\n");
  args[1] = gcc_jit_param_as_rvalue (param_name);

  gcc_jit_block *block = gcc_jit_function_new_block (func, NULL);

  gcc_jit_block_add_comment (
    block, NULL,
    "a test comment");

  gcc_jit_block_add_eval (
    block, NULL,
    gcc_jit_context_new_call (ctxt,
			      NULL,
			      printf_func,
			      2, args));
  gcc_jit_block_end_with_void_return (block, NULL);
}

/* { dg-final { jit-verify-output-file-was-created "" } } */
/* { dg-final { jit-verify-dynamic-library "hello from ./verify-dynamic-library.c.exe" } } */
