#include <stdio.h>
#include "avx512fp16-xmm-check.h"
#include "defines.h"
#include "macros.h"
#include "args.h"

struct IntegerRegisters iregs;
struct FloatRegisters fregs;
unsigned int num_iregs, num_fregs;

__m64
fun_test_returning___m64 (void)
{
  volatile_var++;
  return (__m64){72,0};
}

__m128
fun_test_returning___m128 (void)
{
  volatile_var++;
  return (__m128){73,0,0,0};
}

__m128h
fun_test_returning___m128h (void)
{
  volatile_var++;
  return (__m128h){1.1f16, 2.2f16, 3.3f16, 4.4f16, 5.5f16,
                   6.6f16, 7.7f16, 8.8f16};
}

__m64 test_64;
__m128 test_128;
__m128h test_128h;

static void
do_test (void)
{
  unsigned failed = 0;
  XMM_T xmmt1, xmmt2;

  /* We jump through hoops to compare the results as gcc 3.3 does throw
     an ICE when trying to generate a compare for a == b, when a and b
     are of __m64 or __m128 type :-(  */
  clear_struct_registers;
  test_64 = (__m64){72,0};
  xmmt1._m64[0] = test_64;
  xmmt2._m64[0] = WRAP_RET (fun_test_returning___m64)();
  if (xmmt1._longlong[0] != xmmt2._longlong[0]
      || xmmt1._longlong[0] != xmm_regs[0]._longlong[0])
    printf ("fail m64\n"), failed++;

  clear_struct_registers;
  test_128 = (__m128){73,0};
  xmmt1._m128[0] = test_128;
  xmmt2._m128[0] = WRAP_RET (fun_test_returning___m128)();
  if (xmmt1._longlong[0] != xmmt2._longlong[0]
      || xmmt1._longlong[0] != xmm_regs[0]._longlong[0])
    printf ("fail m128\n"), failed++;

  clear_struct_registers;
  test_128h = (__m128h){1.1f16, 2.2f16, 3.3f16, 4.4f16, 5.5f16,
                        6.6f16, 7.7f16, 8.8f16};
  xmmt1._m128h[0] = test_128h;
  xmmt2._m128h[0] = WRAP_RET (fun_test_returning___m128h)();
  if (xmmt1._longlong[0] != xmmt2._longlong[0]
      || xmmt1._longlong[0] != xmm_regs[0]._longlong[0])
    printf ("fail m128h\n"), failed++;

  if (failed)
    abort ();
}
