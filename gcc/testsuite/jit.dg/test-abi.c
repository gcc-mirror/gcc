/* { dg-do compile { target x86_64-*-* } } */

#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#define TEST_COMPILING_TO_FILE
#define OUTPUT_KIND      GCC_JIT_OUTPUT_KIND_ASSEMBLER
#define OUTPUT_FILENAME  "output-of-test-restrict.c.s"
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
	/* Let's try to inject the equivalent of:
__attribute__((__ms_abi__))
int t(int x, int y) {
  return x * y;
}
	*/
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  gcc_jit_param *x =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "x");
  gcc_jit_param *y =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "y");
  gcc_jit_param *params[2] = {x, y};

  gcc_jit_function *func_t =
    gcc_jit_context_new_function (ctxt, NULL,
      GCC_JIT_FUNCTION_EXPORTED, int_type, "t",
      2, params, 0);
  gcc_jit_function_add_attribute (func_t,
    GCC_JIT_FN_ATTRIBUTE_X86_MS_ABI);

  gcc_jit_block *block = gcc_jit_function_new_block (func_t, NULL);

  gcc_jit_rvalue *result =
    gcc_jit_context_new_binary_op (
      ctxt, NULL, GCC_JIT_BINARY_OP_MULT,
      int_type,
      gcc_jit_param_as_rvalue (x),
      gcc_jit_param_as_rvalue (y));

  gcc_jit_block_end_with_return (block, NULL, result);
}

/* { dg-final { jit-verify-output-file-was-created "" } } */
/* { dg-final { jit-verify-assembler-output "movl\\s+%ecx, %eax" } } */
