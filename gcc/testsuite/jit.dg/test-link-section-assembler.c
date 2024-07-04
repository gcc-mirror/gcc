#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#define TEST_COMPILING_TO_FILE
#define OUTPUT_KIND      GCC_JIT_OUTPUT_KIND_ASSEMBLER
#define OUTPUT_FILENAME  "output-of-test-link-section-assembler.c.s"
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
     int foo __attribute__((section(".section")));
  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_lvalue *foo =
    gcc_jit_context_new_global (
      ctxt, NULL, GCC_JIT_GLOBAL_EXPORTED, int_type, "foo");
  gcc_jit_lvalue_set_link_section(foo, ".my_section");

  gcc_jit_function *func_main =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_type,
				  "main",
				  0, NULL,
				  0);
  gcc_jit_rvalue *zero = gcc_jit_context_zero (ctxt, int_type);
  gcc_jit_block *block = gcc_jit_function_new_block (func_main, NULL);
  gcc_jit_block_end_with_return (block, NULL, zero);
}

/* { dg-final { jit-verify-output-file-was-created "" } } */
/* { dg-final { jit-verify-assembler-output ".section\\s.my_section" } } */
