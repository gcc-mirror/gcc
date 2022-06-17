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
#define OUTPUT_FILENAME  "output-of-test-link-section-assembler.c.s"
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
     register int global_variable asm ("r13");
     int main() {
        register int variable asm ("r12");
        return 0;
     }
  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_lvalue *global_variable =
    gcc_jit_context_new_global (
      ctxt, NULL, GCC_JIT_GLOBAL_EXPORTED, int_type, "global_variable");
  gcc_jit_lvalue_set_register_name(global_variable, "r13");

  gcc_jit_function *func_main =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_type,
				  "main",
				  0, NULL,
				  0);
  gcc_jit_lvalue *variable = gcc_jit_function_new_local(func_main, NULL, int_type, "variable");
  gcc_jit_lvalue_set_register_name(variable, "r12");
  gcc_jit_rvalue *two = gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 2);
  gcc_jit_rvalue *one = gcc_jit_context_one (ctxt, int_type);
  gcc_jit_block *block = gcc_jit_function_new_block (func_main, NULL);
  gcc_jit_block_add_assignment(block, NULL, variable, one);
  gcc_jit_block_add_assignment(block, NULL, global_variable, two);
  gcc_jit_block_end_with_return (block, NULL, gcc_jit_lvalue_as_rvalue(variable));
}

/* { dg-final { jit-verify-output-file-was-created "" } } */
/* { dg-final { jit-verify-assembler-output "movl	\\\$1, %r12d" } } */
/* { dg-final { jit-verify-assembler-output "movl	\\\$2, %r13d" } } */
