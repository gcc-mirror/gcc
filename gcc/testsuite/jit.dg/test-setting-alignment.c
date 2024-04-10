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
#define OUTPUT_FILENAME  "output-of-test-setting-alignment.c.s"
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
     int foo __attribute__((aligned (8)));

     int main (void) {
        int bar __attribute__((aligned (16)));
     }
  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_lvalue *foo =
    gcc_jit_context_new_global (
      ctxt, NULL, GCC_JIT_GLOBAL_EXPORTED, int_type, "foo");
  gcc_jit_lvalue_set_alignment(foo, 8);

  gcc_jit_field *field = gcc_jit_context_new_field (ctxt,
    NULL,
    int_type,
    "a");
  gcc_jit_struct *struct_type =
    gcc_jit_context_new_struct_type(ctxt, NULL, "Type", 1, &field);
  gcc_jit_function *func_main =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_type,
				  "main",
				  0, NULL,
				  0);
  gcc_jit_lvalue *bar =
    gcc_jit_function_new_local (
      func_main, NULL,
      gcc_jit_struct_as_type (struct_type),
      "bar");
  CHECK_VALUE (gcc_jit_lvalue_get_alignment (bar), 0);
  gcc_jit_lvalue_set_alignment (bar, 16);
  CHECK_VALUE (gcc_jit_lvalue_get_alignment (bar), 16);
  gcc_jit_block *block = gcc_jit_function_new_block (func_main, NULL);
  gcc_jit_rvalue *return_value =
      gcc_jit_lvalue_as_rvalue (gcc_jit_lvalue_access_field (bar, NULL, field));
  gcc_jit_block_end_with_return (block, NULL, return_value);
}

/* { dg-final { jit-verify-output-file-was-created "" } } */
/* { dg-final { jit-verify-assembler-output ".comm	foo,4,8" { target { ! *-*-darwin* } } } } */
/* { dg-final { jit-verify-assembler-output ".comm\\s_foo,4,3" { target *-*-darwin* } } } */
/* { dg-final { jit-verify-assembler-output "movl	-16\\\(%rbp\\\), %eax" } } */
