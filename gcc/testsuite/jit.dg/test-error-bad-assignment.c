#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

     void
     test_fn ()
     {
        long integer;
        volatile const void *variable;
        variable = &integer;
        long aligned_integer __attribute__((aligned(4)));
        variable = &aligned_integer;
     }

     and verify that the API complains about the mismatching types
     in the assignments.
  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *long_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_LONG);
  gcc_jit_type *const_void_type =
    gcc_jit_type_get_const (void_type);
  gcc_jit_type *volatile_void_ptr =
    gcc_jit_type_get_pointer (gcc_jit_type_get_volatile (const_void_type));

  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  void_type,
                                  "test_fn",
                                  0, NULL,
                                  0);

  gcc_jit_lvalue *integer = gcc_jit_function_new_local (func, NULL, long_type, "integer");
  gcc_jit_rvalue *address = gcc_jit_lvalue_get_address(integer, NULL);

  gcc_jit_lvalue *variable = gcc_jit_function_new_local (func, NULL, volatile_void_ptr, "variable");
  gcc_jit_block *initial =
    gcc_jit_function_new_block (func, "initial");
  gcc_jit_block_add_assignment(initial, NULL, variable, address);

  gcc_jit_type *aligned_long_type = gcc_jit_type_get_aligned (long_type, 4);
  gcc_jit_lvalue *aligned_integer = gcc_jit_function_new_local (func, NULL, aligned_long_type, "aligned_integer");
  gcc_jit_rvalue *aligned_address = gcc_jit_lvalue_get_address(aligned_integer, NULL);

  gcc_jit_block_add_assignment(initial, NULL, variable, aligned_address);

  gcc_jit_block_end_with_void_return (initial, NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error messages were emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_block_add_assignment:"
		      " mismatching types:"
		      " assignment to variable (type: volatile const void *)"
		      " from &integer (type: long *)");

  CHECK_STRING_VALUE (gcc_jit_context_get_last_error (ctxt),
		      "gcc_jit_block_add_assignment:"
		      " mismatching types:"
		      " assignment to variable (type: volatile const void *)"
		      " from &aligned_integer (type: long  __attribute__((aligned(4))) *)");
}

