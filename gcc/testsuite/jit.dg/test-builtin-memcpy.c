#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
      void *
      test_memcpy (void *dest, const void *src, size_t n)
      {
        return __builtin_memcpy (dest, src, n);
      }
   */
  gcc_jit_type *t_void_ptr
    = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID_PTR);
  gcc_jit_type *t_const_void_ptr
    = gcc_jit_type_get_pointer (gcc_jit_type_get_const
				(gcc_jit_context_get_type
				 (ctxt, GCC_JIT_TYPE_VOID)));
  gcc_jit_type *t_size_t
    = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_SIZE_T);

  gcc_jit_param *dest
    = gcc_jit_context_new_param (ctxt, NULL, t_void_ptr, "dest");
  gcc_jit_param *src
    = gcc_jit_context_new_param (ctxt, NULL, t_const_void_ptr, "src");
  gcc_jit_param *n = gcc_jit_context_new_param (ctxt, NULL, t_size_t, "n");
  gcc_jit_param *params[3] = {dest, src, n};
  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  t_void_ptr,
				  "test_memcpy",
				  3, params, 0);

  gcc_jit_function *jit_memcpy
    = gcc_jit_context_get_builtin_function (ctxt, "__builtin_memcpy");


  gcc_jit_block *b_initial
    = gcc_jit_function_new_block (func, "initial");
  gcc_jit_rvalue *args[3] = {gcc_jit_param_as_rvalue (dest),
			     gcc_jit_param_as_rvalue (src),
			     gcc_jit_param_as_rvalue (n)};
  gcc_jit_rvalue *call
    = gcc_jit_context_new_call (ctxt, NULL, jit_memcpy, 3, args);
  gcc_jit_block_end_with_return (b_initial, NULL, call);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef void *(*test_memcpy_type) (void *, const void *, size_t);
  CHECK_NON_NULL (result);
  test_memcpy_type test_memcpy =
    (test_memcpy_type)gcc_jit_result_get_code (result, "test_memcpy");
  CHECK_NON_NULL (test_memcpy);

  int dst = 13;
  int src = 42;
  void *out = test_memcpy (&dst, &src, sizeof(int));
  CHECK_VALUE(out, &dst);
  CHECK_VALUE(dst, 42);
}
