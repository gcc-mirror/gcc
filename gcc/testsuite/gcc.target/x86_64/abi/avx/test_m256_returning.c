#include <stdio.h>
#include "avx-check.h"
#include "args.h"

struct IntegerRegisters iregs;
struct FloatRegisters fregs;
unsigned int num_iregs, num_fregs;

__m256
fun_test_returning___m256 (void)
{
  volatile_var++;
  return (__m256){73,0,0,0,0,0,0,0};
}

__m256 test_256;

static void
avx_test (void)
{
  unsigned failed = 0;
  YMM_T ymmt1, ymmt2;

  clear_struct_registers;
  test_256 = (__m256){73,0,0,0,0,0,0,0};
  ymmt1._m256[0] = test_256;
  ymmt2._m256[0] = WRAP_RET (fun_test_returning___m256)();
  if (memcmp (&ymmt1, &ymmt2, sizeof (ymmt2)) != 0)
    printf ("fail m256\n"), failed++;
  if (failed)
    abort ();
}
