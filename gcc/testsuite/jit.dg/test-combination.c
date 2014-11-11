/* Construct a test case by combining other test cases, to try to shake
   out state issues: all of the test cases are run in one process, inside
   one gcc_jit_context (per iteration).  */

#include "all-non-failing-tests.h"

/* Now construct a test case from all the other test cases.

   We undefine COMBINED_TEST so that we can now include harness.h
   "for real".  */
#undef COMBINED_TEST
#include "harness.h"

/* Our testing hooks are the combination of the other test cases.  */
void
create_code (gcc_jit_context *ctxt, void * user_data)
{
  create_code_accessing_struct (ctxt, user_data);
  create_code_accessing_union (ctxt, user_data);
  create_code_array_as_pointer (ctxt, user_data);
  create_code_arrays (ctxt, user_data);
  create_code_calling_external_function (ctxt, user_data);
  create_code_calling_function_ptr (ctxt, user_data);
  create_code_dot_product (ctxt, user_data);
  create_code_expressions (ctxt, user_data);
  create_code_factorial (ctxt, user_data);
  create_code_fibonacci (ctxt, user_data);
  create_code_functions (ctxt, user_data);
  create_code_hello_world (ctxt, user_data);
  create_code_linked_list (ctxt, user_data);
  create_code_long_names (ctxt, user_data);
  create_code_quadratic (ctxt, user_data);
  create_code_nested_loop (ctxt, user_data);
  create_code_reading_struct  (ctxt, user_data);
  create_code_string_literal (ctxt, user_data);
  create_code_sum_of_squares (ctxt, user_data);
  create_code_types (ctxt, user_data);
  create_code_using_global (ctxt, user_data);
  create_code_volatile (ctxt, user_data);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  verify_code_accessing_struct (ctxt, result);
  verify_code_accessing_union (ctxt, result);
  verify_code_array_as_pointer (ctxt, result);
  verify_code_arrays (ctxt, result);
  verify_code_calling_external_function (ctxt, result);
  verify_code_calling_function_ptr (ctxt, result);
  verify_code_dot_product (ctxt, result);
  verify_code_expressions (ctxt, result);
  verify_code_factorial (ctxt, result);
  verify_code_fibonacci (ctxt, result);
  verify_code_functions (ctxt, result);
  verify_code_hello_world (ctxt, result);
  verify_code_linked_list (ctxt, result);
  verify_code_long_names (ctxt, result);
  verify_code_quadratic (ctxt, result);
  verify_code_nested_loop (ctxt, result);
  verify_code_reading_struct (ctxt, result);
  verify_code_string_literal (ctxt, result);
  verify_code_sum_of_squares (ctxt, result);
  verify_code_types (ctxt, result);
  verify_code_using_global (ctxt, result);
  verify_code_volatile (ctxt, result);
}
