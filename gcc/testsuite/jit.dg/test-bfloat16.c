/* { dg-do compile { target x86_64-*-* } } */

#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

/* We don't want set_options() in harness.h to set -O3 so our little local
   is optimized away. */
#define TEST_ESCHEWS_SET_OPTIONS
static void set_options (gcc_jit_context *ctxt, const char *argv0)
{
}

#define TEST_COMPILING_TO_FILE
#define OUTPUT_KIND      GCC_JIT_OUTPUT_KIND_ASSEMBLER
#define OUTPUT_FILENAME  "output-of-test-bfloat16.c.s"
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_type *bf16_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_BFLOAT16);

  gcc_jit_lvalue *foo =
    gcc_jit_context_new_global (
      ctxt, NULL, GCC_JIT_GLOBAL_EXPORTED, bf16_type, "foo");

  gcc_jit_rvalue *value =
    gcc_jit_context_new_rvalue_from_double (ctxt, bf16_type, 3.1415);
  gcc_jit_global_set_initializer_rvalue (foo, value);
}

/* { dg-final { jit-verify-output-file-was-created "" } } */
/* { dg-final { jit-verify-assembler-output ".value	16457" } } */
/* { dg-final { jit-verify-assembler-output ".size	foo, 2" } } */
