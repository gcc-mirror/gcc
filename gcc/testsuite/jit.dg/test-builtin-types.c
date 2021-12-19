#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  CHECK_NON_NULL (gcc_jit_context_get_builtin_function (ctxt, "__atomic_fetch_add_4"));

  gcc_jit_function *atomic_load = gcc_jit_context_get_builtin_function (ctxt, "__atomic_load_8");

  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *const_void_type =
    gcc_jit_type_get_const (gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID));
  gcc_jit_type *volatile_void_ptr =
    gcc_jit_type_get_pointer (gcc_jit_type_get_volatile (const_void_type));
  gcc_jit_type *long_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_LONG);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL, GCC_JIT_FUNCTION_EXPORTED, void_type, "atomics", 0, NULL, 0);

  gcc_jit_lvalue *variable = gcc_jit_function_new_local (func, NULL, long_type, "variable");
  gcc_jit_rvalue *builtin_args[2];
  gcc_jit_rvalue *param1 = gcc_jit_lvalue_get_address(variable, NULL);
  builtin_args[0] = gcc_jit_context_new_cast(ctxt, NULL, param1, volatile_void_ptr);
  builtin_args[1] = gcc_jit_context_new_rvalue_from_long(ctxt, int_type, 0);
  gcc_jit_context_new_call (ctxt, NULL, atomic_load, 2, builtin_args);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  /* Verify that no errors were emitted.  */
  CHECK_NON_NULL (result);
}
