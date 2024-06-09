/* { dg-do compile { target x86_64-*-* } } */

#include <assert.h>
#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *int_ptr_type = gcc_jit_type_get_pointer (int_type);

  int int_ptr_size = gcc_jit_type_get_size (int_ptr_type);
  CHECK_VALUE (int_ptr_size, 8);

  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *void_ptr_type = gcc_jit_type_get_pointer (void_type);

  CHECK_VALUE (int_ptr_size, gcc_jit_type_get_size (void_ptr_type));
}
