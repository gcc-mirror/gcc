/* Test of using the API with very long names.  */

#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

/* 65KB */
#define NAME_LENGTH (65 * 1024)

static struct long_names
{
  char struct_name[NAME_LENGTH];
  char fn_name[NAME_LENGTH];
  char local_name[NAME_LENGTH];
  char block_name[NAME_LENGTH];
} long_names;

static void
populate_name (const char *prefix, char *buffer)
{
  int i;

  /* Begin with the given prefix: */
  sprintf (buffer, "%s", prefix);

  /* Populate the rest of the buffer with 0123456789 repeatedly: */
  for (i = strlen (prefix); i < NAME_LENGTH - 1; i++)
    buffer[i] = '0' + (i % 10);

  /* NIL-terminate the buffer: */
  buffer[NAME_LENGTH - 1] = '\0';
}

static void
populate_names (void)
{
  populate_name ("struct_", long_names.struct_name);
  populate_name ("test_fn_", long_names.fn_name);
  populate_name ("local_", long_names.local_name);
  populate_name ("block_", long_names.block_name);
}

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Where "ETC" is a very long suffix, let's try to inject the
     equivalent of:

       struct struct_ETC;

       int
       test_fn_ETC ()
       {
	  int local_ETC;
	  local_ETC = 42;
	  return local_ETC;
       }

     to verify that the API copes with such long names.  */

  populate_names ();

  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  /* We don't yet use this struct.  */
  (void)gcc_jit_context_new_opaque_struct (ctxt, NULL,
					   long_names.struct_name);

  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_type,
				  long_names.fn_name,
				  0, NULL,
				  0);
  gcc_jit_lvalue *local =
    gcc_jit_function_new_local (test_fn,
				NULL,
				int_type,
				long_names.local_name);

  gcc_jit_block *block =
    gcc_jit_function_new_block (test_fn, long_names.block_name);

  gcc_jit_block_add_assignment (
    block,
    NULL,
    local,
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 42));

  gcc_jit_block_end_with_return (
    block, NULL,
    gcc_jit_lvalue_as_rvalue (local));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_NON_NULL (result);

  typedef int (*my_fn_type) (void);
  CHECK_NON_NULL (result);
  my_fn_type my_fn =
    (my_fn_type)gcc_jit_result_get_code (result, long_names.fn_name);
  CHECK_NON_NULL (my_fn);
  int val = my_fn ();
  CHECK_VALUE (val, 42);
}
