#include "bf16-check.h"
#include "defines.h"
#include "macros.h"
#include "args.h"

__bf16
fun_test_returning_bf16 (void)
{
  __bf16 b = make_f32_bf16 (72.0f);
  volatile_var++;
  return b;
}

static void
do_test (void)
{
  __bf16 var = WRAP_RET (fun_test_returning_bf16) ();
  assert (check_bf16_float (xmm_regs[0].___bf16[0], 72.0f) == 1);
  assert (check_bf16_float (var, 72.0f) == 1);
}
