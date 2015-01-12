#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

     void
     fn_one ()
     {
       int i;
     }

     int
     fn_two ()
     {
       return i;
     }

     and verify that the API complains about the use of the local
     from the other function.  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  gcc_jit_function *fn_one =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  void_type,
				  "fn_one",
				  0, NULL,
				  0);
  gcc_jit_lvalue *local =
    gcc_jit_function_new_local (fn_one, NULL, int_type, "i");

  gcc_jit_function *fn_two =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  int_type,
                                  "fn_two",
                                  0, NULL,
                                  0);

  gcc_jit_block *block = gcc_jit_function_new_block (fn_two, NULL);
  /* "return i;", using local i from the wrong function.  */
  gcc_jit_block_end_with_return (block,
				 NULL,
				 gcc_jit_lvalue_as_rvalue (local));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      ("gcc_jit_block_end_with_return:"
		       " rvalue i (type: int)"
		       " has scope limited to function fn_one"
		       " but was used within function fn_two"
		       " (in statement: return i;)"));
}

