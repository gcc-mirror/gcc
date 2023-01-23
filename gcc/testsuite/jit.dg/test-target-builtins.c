/* { dg-do compile { target x86_64-*-* } } */

#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#define TEST_PROVIDES_MAIN
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  CHECK_NON_NULL (gcc_jit_context_get_target_builtin_function (ctxt, "__builtin_ia32_xgetbv"));
  gcc_jit_function *builtin_eh_pointer = gcc_jit_context_get_target_builtin_function (ctxt, "__builtin_eh_pointer");
  CHECK_NON_NULL (builtin_eh_pointer);

  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *void_ptr =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID_PTR);
  gcc_jit_function *func_main =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_type,
				  "main",
				  0, NULL,
				  0);
  gcc_jit_rvalue *zero = gcc_jit_context_zero (ctxt, int_type);
  gcc_jit_block *block = gcc_jit_function_new_block (func_main, NULL);
  gcc_jit_lvalue *variable = gcc_jit_function_new_local(func_main, NULL, void_ptr, "variable");
  gcc_jit_block_add_assignment (block, NULL, variable,
    gcc_jit_context_new_call (ctxt, NULL, builtin_eh_pointer, 1, &zero));
  gcc_jit_block_end_with_return (block, NULL, zero);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_NON_NULL (result);
}

int
main (int argc, char **argv)
{
  /*  This is the same as the main provided by harness.h, but it first create a dummy context and compile
      in order to add the target builtins to libgccjit's internal state.  */
  gcc_jit_context *ctxt;
  ctxt = gcc_jit_context_acquire ();
  if (!ctxt)
    {
      fail ("gcc_jit_context_acquire failed");
      return -1;
    }
  gcc_jit_result *result;
  result = gcc_jit_context_compile (ctxt);
  gcc_jit_result_release (result);
  gcc_jit_context_release (ctxt);

  int i;

  for (i = 1; i <= 5; i++)
    {
      snprintf (test, sizeof (test),
		"%s iteration %d of %d",
                extract_progname (argv[0]),
                i, 5);

      //printf ("ITERATION %d\n", i);
      test_jit (argv[0], NULL);
      //printf ("\n");
    }

  totals ();

  return 0;
}
