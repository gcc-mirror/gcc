#include <stdio.h>
#include "bf16-check.h"
#include "defines.h"
#include "macros.h"
#include "args.h"

struct FloatRegisters fregs;
struct IntegerRegisters iregs;
unsigned int num_fregs, num_iregs;

volatile __bf16 bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8;

__m128bf16
fun_test_returning___m128bf16 (void)
{
  volatile_var++;
  return (__m128bf16) { bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8 };
}

__m128bf16 test_128bf16;

static void
do_test (void)
{
  unsigned failed = 0;
  XMM_T xmmt1, xmmt2;

  clear_struct_registers;
  test_128bf16 = (__m128bf16) { bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8 };
  xmmt1._m128bf16[0] = test_128bf16;
  xmmt2._m128bf16[0] = WRAP_RET (fun_test_returning___m128bf16)();
  if (xmmt1._longlong[0] != xmmt2._longlong[0]
      || xmmt1._longlong[0] != xmm_regs[0]._longlong[0])
    printf ("fail m128bf16\n"), failed++;

  if (failed)
    abort ();
}
