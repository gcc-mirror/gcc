/* { dg-do compile { target x86_64-*-* } } */

#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#define TEST_ESCHEWS_SET_OPTIONS
static void set_options (gcc_jit_context *ctxt, const char *argv0)
{
  // Set "-O3".
  gcc_jit_context_set_int_option(ctxt, GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL, 3);
}

#define TEST_COMPILING_TO_FILE
#define OUTPUT_KIND      GCC_JIT_OUTPUT_KIND_ASSEMBLER
#define OUTPUT_FILENAME  "output-of-test-pure-attribute.c.s"
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
__attribute__ ((pure))
int foo (int x);
int xxx(void)
{
  int x = 45;
  int sum = 0;

  while (x >>= 1)
    sum += foo (x) * 2;
  return sum;
}
  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  /* Creating the `foo` function. */
  gcc_jit_param *n =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "x");
  gcc_jit_param *params[1] = {n};
  gcc_jit_function *foo_func =
    gcc_jit_context_new_function (ctxt, NULL,
          GCC_JIT_FUNCTION_IMPORTED,
          int_type,
          "foo",
          1, params,
          0);
  gcc_jit_function_add_attribute(foo_func, GCC_JIT_FN_ATTRIBUTE_PURE);

  /* Creating the `xxx` function. */
  gcc_jit_function *xxx_func =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_type,
				  "xxx",
				  0, NULL,
				  0);

  gcc_jit_block *block = gcc_jit_function_new_block (xxx_func, NULL);

  /* Build locals:  */
  gcc_jit_lvalue *x =
    gcc_jit_function_new_local (xxx_func, NULL, int_type, "x");
  gcc_jit_lvalue *sum =
    gcc_jit_function_new_local (xxx_func, NULL, int_type, "sum");

  /* int x = 45 */
  gcc_jit_block_add_assignment (
    block, NULL,
    x,
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 45));
  /* int sum = 0 */
  gcc_jit_block_add_assignment (
    block, NULL,
    sum,
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 0));

  /* while (x >>= 1) { sum += foo (x) * 2; } */
  gcc_jit_block *loop_cond =
    gcc_jit_function_new_block (xxx_func, "loop_cond");
  gcc_jit_block *loop_body =
    gcc_jit_function_new_block (xxx_func, "loop_body");
  gcc_jit_block *after_loop =
    gcc_jit_function_new_block (xxx_func, "after_loop");

  gcc_jit_block_end_with_jump (block, NULL, loop_cond);


  /* if (x >>= 1) */
  /* Since gccjit doesn't (yet?) have support for `>>=` operator, we will decompose it into:
     `if (x = x >> 1)` */
  gcc_jit_block_add_assignment_op (
    loop_cond, NULL,
    x,
    GCC_JIT_BINARY_OP_RSHIFT,
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 1));
  /* The condition itself */
  gcc_jit_block_end_with_conditional (
    loop_cond, NULL,
    gcc_jit_context_new_comparison (
       ctxt, NULL,
       GCC_JIT_COMPARISON_NE,
       gcc_jit_lvalue_as_rvalue (x),
       gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 0)),
    after_loop,
    loop_body);

  /* sum += foo (x) * 2; */
  gcc_jit_rvalue *arg = gcc_jit_lvalue_as_rvalue(x);
  gcc_jit_block_add_assignment_op (
    loop_body, NULL,
    x,
    GCC_JIT_BINARY_OP_PLUS,
    gcc_jit_context_new_binary_op (
      ctxt, NULL,
      GCC_JIT_BINARY_OP_MULT, int_type,
      gcc_jit_context_new_call (ctxt, NULL, foo_func, 1, &arg),
      gcc_jit_context_new_rvalue_from_int (
	ctxt,
	int_type,
	2)));
  gcc_jit_block_end_with_jump (loop_body, NULL, loop_cond);

  /* return sum; */
  gcc_jit_block_end_with_return (after_loop, NULL, gcc_jit_lvalue_as_rvalue(sum));
}

/* { dg-final { jit-verify-output-file-was-created "" } } */
/* Check that the loop was optimized away */
/* { dg-final { jit-verify-assembler-output-not "jne" } } */
