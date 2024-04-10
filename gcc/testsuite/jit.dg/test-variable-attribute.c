/* { dg-do compile { target x86_64-*-* } } */

#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#define TEST_COMPILING_TO_FILE
#define OUTPUT_KIND      GCC_JIT_OUTPUT_KIND_ASSEMBLER
#define OUTPUT_FILENAME  "output-of-test-variable-attribute.c.s"
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

int PRIVATE __attribute__ ((visibility ("hidden"))) = 42;
int PUBLIC = 12;
  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  /* Creating the `PRIVATE` variable. */
  gcc_jit_lvalue *private = gcc_jit_context_new_global (ctxt,
    NULL, GCC_JIT_GLOBAL_EXPORTED, int_type, "PRIVATE");
  gcc_jit_lvalue_add_string_attribute (private,
    GCC_JIT_VARIABLE_ATTRIBUTE_VISIBILITY, "hidden");
  gcc_jit_rvalue *rval = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 42);
  gcc_jit_global_set_initializer_rvalue (private, rval);

  /* Creating the `PUBLIC` variable. */
  gcc_jit_lvalue *public = gcc_jit_context_new_global (ctxt,
    NULL, GCC_JIT_GLOBAL_EXPORTED, int_type, "PUBLIC");
  gcc_jit_rvalue *rval2 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 12);
  gcc_jit_global_set_initializer_rvalue (public, rval2);
}

/* { dg-final { jit-verify-output-file-was-created "" } } */
/* Check that the attribute was applied correctly */
/* { dg-final { jit-verify-assembler-output ".hidden\\s+PRIVATE" { target { ! *-*-darwin* } } } } */
/* { dg-final { jit-verify-assembler-output ".private_extern\\s+_PRIVATE" { target *-*-darwin* } } } */
/* { dg-final { jit-verify-assembler-output ".globl\\s+_?PRIVATE" } } */
/* { dg-final { jit-verify-assembler-output-not ".hidden\\s+PUBLIC" { target { ! *-*-darwin* } } } } */
/* { dg-final { jit-verify-assembler-output-not ".private_extern\\s+_PUBLIC" { target *-*-darwin* } } } */
/* { dg-final { jit-verify-assembler-output ".globl\\s+_?PUBLIC" } } */
