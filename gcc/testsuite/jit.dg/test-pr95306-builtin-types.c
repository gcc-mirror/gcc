#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
#define CHECK_BUILTIN(NAME) \
  CHECK_NON_NULL (gcc_jit_context_get_builtin_function (ctxt, NAME));
  
  CHECK_BUILTIN ("__atomic_load");
  CHECK_BUILTIN ("__builtin_memcpy");
  CHECK_BUILTIN ("__builtin_sadd_overflow");

#undef CHECK_BUILTIN
}

extern void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  /* Verify that no errors were emitted.  */
  CHECK_NON_NULL (result);
}
