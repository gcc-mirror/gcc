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
#define OUTPUT_FILENAME  "output-of-test-noinline-attribute.c.s"
#include "harness.h"

gcc_jit_function*
create_function (gcc_jit_context *ctxt,
		 const char *func_name,
		 gcc_jit_type *int_type,
		 int returned_value)
{
  gcc_jit_function *func
    = gcc_jit_context_new_function(ctxt, NULL,
	  GCC_JIT_FUNCTION_INTERNAL,
	  int_type,
	  func_name,
	  0, NULL,
	  0);
  gcc_jit_block *block = gcc_jit_function_new_block (func, NULL);

  gcc_jit_block_add_extended_asm (block, NULL, "");
  gcc_jit_block_end_with_return (block, NULL,
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, returned_value));

  return func;
}


void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
__attribute__ ((noinline))
static int not_removed() {
  asm("");
  return 1;
}
static int removed() {
  asm("");
  return 2;
}
int foo () {
  int x = 0;
  x += removed();
  x += not_removed();
  return x;
}
  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  /* Creating the `not_removed` function. */
  gcc_jit_function *not_removed_func =
    create_function (ctxt, "not_removed", int_type, 1);
  /* __attribute__ ((no_inline)) */
  gcc_jit_function_add_attribute(not_removed_func, GCC_JIT_FN_ATTRIBUTE_NOINLINE);

  /* Creating the `removed` function. */
  gcc_jit_function *removed_func =
    create_function (ctxt, "removed", int_type, 2);

  /* Creating the `foo` function. */
  gcc_jit_function *foo_func =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_type,
				  "foo",
				  0, NULL,
				  0);

  gcc_jit_block *foo_block = gcc_jit_function_new_block (foo_func, NULL);

  /* Build locals:  */
  gcc_jit_lvalue *x =
    gcc_jit_function_new_local (foo_func, NULL, int_type, "x");

  /* int x = 0; */
  gcc_jit_block_add_assignment (
    foo_block, NULL,
    x,
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 0));

  /* x += removed(); */
  gcc_jit_block_add_assignment_op (
    foo_block, NULL,
    x,
    GCC_JIT_BINARY_OP_PLUS,
    gcc_jit_context_new_call (ctxt, NULL, removed_func, 0, NULL));
  
  /* x += not_removed(); */
  gcc_jit_block_add_assignment_op (
    foo_block, NULL,
    x,
    GCC_JIT_BINARY_OP_PLUS,
    gcc_jit_context_new_call (ctxt, NULL, not_removed_func, 0, NULL));

  /* return x; */
  gcc_jit_block_end_with_return (foo_block, NULL, gcc_jit_lvalue_as_rvalue(x));
}

/* { dg-final { jit-verify-output-file-was-created "" } } */
/* Check that the "removed" function was inlined, but not the others */
/* { dg-final { jit-verify-assembler-output-not ".type\\s+removed.isra.0,\\s+@function" { target { ! *-*-darwin* } } } } */
/* { dg-final { jit-verify-assembler-output ".type\\s+not_removed.isra.0,\\s+@function" { target { ! *-*-darwin* } } } } */
/* { dg-final { jit-verify-assembler-output ".type\\s+foo,\\s+@function" { target { ! *-*-darwin* } } } } */

/* { dg-final { jit-verify-assembler-output-not "\\n_removed.isra.0:" { target *-*-darwin* } } } */
/* { dg-final { jit-verify-assembler-output "\\n_not_removed.isra.0:" { target *-*-darwin* } } } */
/* { dg-final { jit-verify-assembler-output "\\n_foo:" { target *-*-darwin* } } } */
