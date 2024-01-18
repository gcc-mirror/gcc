#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include "libgccjit.h"

#define TEST_COMPILING_TO_FILE
#define OUTPUT_KIND      GCC_JIT_OUTPUT_KIND_ASSEMBLER
#define OUTPUT_FILENAME  "output-of-test-test-temp.c.s"
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
int
func ()
{
   int temp = 10;
   return temp;
}
   */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt,
				  NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_type,
				  "func",
				  0, NULL, 0);

  gcc_jit_block *initial =
    gcc_jit_function_new_block (func, "initial");

  gcc_jit_lvalue *temp =
    gcc_jit_function_new_temp (func, NULL, int_type);

  gcc_jit_rvalue *ten =
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 10);
  gcc_jit_block_add_assignment (initial, NULL, temp, ten);

  gcc_jit_block_end_with_return(initial, NULL,
    gcc_jit_lvalue_as_rvalue (temp));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_NON_NULL (result);
}

/* { dg-final { jit-verify-output-file-was-created "" } } */
/* { dg-final { jit-verify-assembler-output-not "JITTMP" } } */
