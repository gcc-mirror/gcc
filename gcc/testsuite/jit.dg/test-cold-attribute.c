/* { dg-do compile { target x86_64-*-* } } */

#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#define TEST_ESCHEWS_SET_OPTIONS
static void set_options (gcc_jit_context *ctxt, const char *argv0)
{
  // Set "-O2".
  gcc_jit_context_set_int_option(ctxt, GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL, 2);
}

#define TEST_COMPILING_TO_FILE
#define OUTPUT_KIND      GCC_JIT_OUTPUT_KIND_ASSEMBLER
#define OUTPUT_FILENAME  "output-of-test-cold-attribute.c.s"
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
int
__attribute__ ((cold))
t()
{
  return -1;
}

  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  gcc_jit_function *func_t =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_type,
				  "t",
				  0, NULL,
				  0);
  gcc_jit_function_add_attribute(func_t, GCC_JIT_FN_ATTRIBUTE_COLD);
  gcc_jit_block *block = gcc_jit_function_new_block (func_t, NULL);
  gcc_jit_rvalue *ret = gcc_jit_context_new_rvalue_from_int (ctxt,
    int_type,
    -1);

  gcc_jit_block_end_with_return (block, NULL, ret);
}

/* { dg-final { jit-verify-output-file-was-created "" } } */
/* { dg-final { jit-verify-assembler-output "orl" } } */
