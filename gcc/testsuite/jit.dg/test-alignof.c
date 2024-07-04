#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
int
my_alignof ()
{
   return _Alignof(int32_t);
}
   */
  gcc_jit_type *int32 =
    gcc_jit_context_get_int_type (ctxt, 4, 1);
  gcc_jit_type *int64 =
    gcc_jit_context_get_int_type (ctxt, 8, 1);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt,
				  NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_type,
				  "my_alignof",
				  0, NULL, 0);

  gcc_jit_block *initial =
    gcc_jit_function_new_block (func, "initial");

  gcc_jit_field *field1 =
    gcc_jit_context_new_field (ctxt, NULL, int32, "int32");
  gcc_jit_field *field2 =
    gcc_jit_context_new_field (ctxt, NULL, int64, "int64");

  gcc_jit_field *fields[] = {
    field1,
    field2,
  };

  gcc_jit_struct *struct_type =
    gcc_jit_context_new_struct_type (
      ctxt,
      NULL,
      "ints",
      2, fields);

  gcc_jit_block_end_with_return(initial, NULL,
    gcc_jit_context_new_alignof(ctxt, gcc_jit_struct_as_type (struct_type)));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef int (*my_alignof_type) ();
  CHECK_NON_NULL (result);
  my_alignof_type my_alignof =
    (my_alignof_type)gcc_jit_result_get_code (result, "my_alignof");
  CHECK_NON_NULL (my_alignof);
  int val = my_alignof ();
  CHECK_VALUE (val, 8);
}
