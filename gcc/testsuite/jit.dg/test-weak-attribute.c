/* { dg-do compile { target x86_64-*-* } } */

#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#define TEST_COMPILING_TO_FILE
#define OUTPUT_KIND      GCC_JIT_OUTPUT_KIND_ASSEMBLER
#define OUTPUT_FILENAME  "output-of-test-weak-attribute.c.s"
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

__attribute__ ((weak))
void f () {}
  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);

  /* Creating the `f` function. */
  gcc_jit_function *f_func =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  void_type,
				  "f",
				  0, NULL,
				  0);
  gcc_jit_function_add_attribute(f_func, GCC_JIT_FN_ATTRIBUTE_WEAK);

  /* void f () {} */
  gcc_jit_block *block = gcc_jit_function_new_block (f_func, NULL);
  gcc_jit_block_end_with_void_return (block, NULL);
}

/* { dg-final { jit-verify-output-file-was-created "" } } */
/* Check that the attribute was applied correctly */
/* { dg-final { jit-verify-assembler-output ".weak\\s+f" { target { ! *-*-darwin* } } } } */
/* { dg-final { jit-verify-assembler-output ".weak_definition\\s+_f" { target *-*-darwin* } } } */
