#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
       char
       test_array_bounds (void)
       {
	  char buffer[10];
	  return buffer[10];
       }
     with -Warray-bounds and -ftree-vrp and verify that the
     out-of-bounds access is detected and reported as a jit error.  */
  gcc_jit_context_add_command_line_option (ctxt, "-Warray-bounds");
  gcc_jit_context_add_command_line_option (ctxt, "-ftree-vrp");

  /* Ensure that the error message doesn't contain colorization codes
     or escaped URLs, even if run at a TTY.  */
  gcc_jit_context_add_command_line_option (ctxt, "-fdiagnostics-color=never");
  gcc_jit_context_add_command_line_option (ctxt, "-fdiagnostics-urls=never");

  gcc_jit_type *char_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_CHAR);
  gcc_jit_type *array_type =
    gcc_jit_context_new_array_type (ctxt, NULL,
				    char_type, 10);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  /* Build the test_fn.  */
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  char_type,
				  "test_array_bounds",
				  0, NULL,
				  0);
  gcc_jit_lvalue *buffer =
    gcc_jit_function_new_local (test_fn, NULL, array_type, "buffer");

  /* tree-vrp.c:check_all_array_refs only checks array lookups that
     have source locations.  */
  gcc_jit_location *dummy_loc =
    gcc_jit_context_new_location (ctxt, "dummy.c", 10, 4);

  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);
  gcc_jit_rvalue *index =
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 10);
  gcc_jit_lvalue *read_of_the_buffer =
    gcc_jit_context_new_array_access (ctxt, dummy_loc,
				      gcc_jit_lvalue_as_rvalue (buffer),
				      index);
  gcc_jit_block_end_with_return (
    block, dummy_loc,
    gcc_jit_lvalue_as_rvalue (read_of_the_buffer));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  /* Verify that the diagnostic led to the context failing... */
  CHECK_VALUE (result, NULL);

  /* ...and that the message was captured by the API.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "array subscript 10 is above array bounds of"
		      " 'char[10]' [-Warray-bounds]");
}
