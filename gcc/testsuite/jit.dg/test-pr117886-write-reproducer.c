/* Verify that we can generate and compile reproducers for
   gcc_jit_context_new_array_constructor
   when the values are lvalues */

#include <stdio.h>
#include <string.h>

#include "libgccjit.h"
#include "harness.h"

/* 
   int foo[3];

   void test (void)
   {
     int a = 1;
     int b = 2;
     int c = 3;
     foo = {a,b,c};
   }
*/

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_type *int_type
    = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *void_type
    = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);

  gcc_jit_type *arr_type = gcc_jit_context_new_array_type (ctxt,
							   0,
							   int_type,
							   3);
  gcc_jit_lvalue *global_intarr_123 = gcc_jit_context_new_global (
								  ctxt, NULL,
								  GCC_JIT_GLOBAL_EXPORTED,
								  arr_type,
								  "global_intarr_123");

  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  void_type,
				  "initialize_intarr_123",
				  0, NULL, 0);
  gcc_jit_lvalue *a
    = gcc_jit_function_new_local (func, NULL, int_type, "a");
  gcc_jit_lvalue *b
    = gcc_jit_function_new_local (func, NULL, int_type, "b");
  gcc_jit_lvalue *c
    = gcc_jit_function_new_local (func, NULL, int_type, "c");

  gcc_jit_block *bb =
    gcc_jit_function_new_block (func, "initial");

  gcc_jit_block_add_assignment (bb, NULL,
				a, gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 1));
  gcc_jit_block_add_assignment (bb, NULL,
				b, gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 2));
  gcc_jit_block_add_assignment (bb, NULL,
				c, gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 3));
     
  gcc_jit_rvalue *values[] = {(gcc_jit_rvalue *)a,
			      (gcc_jit_rvalue *)b,
			      (gcc_jit_rvalue *)c};
  gcc_jit_rvalue *ctor = gcc_jit_context_new_array_constructor (ctxt,
								0,
								arr_type,
								3,
								values);
  gcc_jit_block_add_assignment (bb, NULL,
				global_intarr_123,
				ctor);

  gcc_jit_block_end_with_void_return (bb, NULL);  
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_NON_NULL (result);

  typedef void (*my_fn_type) (void);
  my_fn_type initialize_intarr_123
    = (my_fn_type)gcc_jit_result_get_code (result, "initialize_intarr_123");
  CHECK_NON_NULL (initialize_intarr_123);

  {
    int *foo = gcc_jit_result_get_global (result, "global_intarr_123");
    CHECK_NON_NULL (foo);

    CHECK_VALUE (foo[0], 0);
    CHECK_VALUE (foo[1], 0);
    CHECK_VALUE (foo[2], 0);

    initialize_intarr_123 ();

    CHECK_VALUE (foo[0], 1);
    CHECK_VALUE (foo[1], 2);
    CHECK_VALUE (foo[2], 3);
  }
}
