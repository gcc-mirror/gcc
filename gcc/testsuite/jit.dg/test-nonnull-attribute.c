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
#define OUTPUT_FILENAME  "output-of-test-nonnull.c.s"
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

__attribute__((nonnull(1)))
int t(int *a) {
  if (!a) {
    return -1;
  }
  return *a;
}
  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *pint_type = gcc_jit_type_get_pointer(int_type);

  gcc_jit_param *a =
    gcc_jit_context_new_param (ctxt, NULL, pint_type, "a");

  gcc_jit_function *func_t =
    gcc_jit_context_new_function (ctxt, NULL,
	  GCC_JIT_FUNCTION_EXPORTED,
	  int_type,
	  "t",
	  1, &a,
	  0);
  /* Adding `nonnull(1)` attribute. */
  int indexes[1] = {1};
  gcc_jit_function_add_integer_array_attribute (
    func_t,
    GCC_JIT_FN_ATTRIBUTE_NONNULL,
    indexes,
    1
  );

  /* if (!a) {
    return -1;
  } */
  gcc_jit_block *if_cond =
    gcc_jit_function_new_block (func_t, "if_cond");
  gcc_jit_block *if_body =
    gcc_jit_function_new_block (func_t, "if_body");
  gcc_jit_block *after_if =
    gcc_jit_function_new_block (func_t, "after_if");

  /* if (!a) */
  gcc_jit_block_end_with_conditional (
    if_cond, NULL,
    gcc_jit_context_new_comparison (
      ctxt, NULL,
      GCC_JIT_COMPARISON_EQ,
      gcc_jit_param_as_rvalue (a),
      gcc_jit_context_null (ctxt, pint_type)),
    if_body,
    after_if);
  /* return -1; */
  gcc_jit_block_end_with_return (
    if_body, NULL,
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, -1));

  /* return *a; */
  gcc_jit_block_end_with_return (
    after_if, NULL,
    gcc_jit_lvalue_as_rvalue (
      gcc_jit_rvalue_dereference (
	gcc_jit_param_as_rvalue (a), NULL)));
}

/* { dg-final { jit-verify-output-file-was-created "" } } */
/* Check that the "if block" was optimized away */
/* { dg-final { jit-verify-assembler-output-not "testq" } } */
/* { dg-final { jit-verify-assembler-output-not "-1" } } */
