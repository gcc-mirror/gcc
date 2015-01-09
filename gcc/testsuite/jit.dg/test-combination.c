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
  for (int i = 0; i < num_testcases; i++)
    testcases[i].m_hook_to_create_code (ctxt, user_data);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  for (int i = 0; i < num_testcases; i++)
    testcases[i].m_hook_to_verify_code (ctxt, result);
}
