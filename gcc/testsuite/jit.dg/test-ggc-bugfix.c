/* { dg-do compile { target x86_64-*-* } } */

#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_context_add_command_line_option (ctxt, "-flto");
  gcc_jit_context_add_driver_option (ctxt, "-nostdlib");
#if __APPLE__
  /* On newer macOS, the test will fail with a complaint from the linker about
     all user-land exes needing libSystem, so add it.  */
  gcc_jit_context_add_driver_option (ctxt, "-lSystem");
#endif

  gcc_jit_type *type_int = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_param *params_for_func_main[0] = {
  };
  gcc_jit_function *func_main =
    gcc_jit_context_new_function (ctxt, NULL, GCC_JIT_FUNCTION_EXPORTED,
                                  type_int, "main", 0, params_for_func_main,
                                  0);
  gcc_jit_block *block_start =
    gcc_jit_function_new_block (func_main, "start");
  gcc_jit_rvalue *rvalue__int_42 =
    gcc_jit_context_new_rvalue_from_int (ctxt, type_int, 42);
  gcc_jit_block_end_with_return (block_start, NULL, rvalue__int_42);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_NON_NULL (result);
}
