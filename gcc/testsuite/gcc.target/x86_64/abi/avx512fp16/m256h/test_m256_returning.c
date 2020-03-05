#include <stdio.h>
#include "avx512fp16-ymm-check.h"
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

__m256h
fun_test_returning___m256h (void)
{
  volatile_var++;
  return (__m256h){1.1f16,2.1f16,3.1f16,4.1f16,
                   5.1f16,6.1f16,7.1f16,8.1f16,
                   9.1f16,10.1f16,11.1f16,12.1f16,
		   13.1f16,14.1f16,15.1f16,16.1f16};
}

__m256 test_256;
__m256h test_256h;

static void
do_test (void)
{
  unsigned failed = 0;
  YMM_T ymmt1, ymmt2;

  clear_struct_registers;
  test_256 = (__m256){73,0,0,0,0,0,0,0};
  ymmt1._m256[0] = test_256;
  ymmt2._m256[0] = WRAP_RET (fun_test_returning___m256)();
  if (memcmp (&ymmt1, &ymmt2, sizeof (ymmt2)) != 0)
    printf ("fail m256\n"), failed++;

  clear_struct_registers;
  test_256h = (__m256h){1.1f16,2.1f16,3.1f16,4.1f16,
                        5.1f16,6.1f16,7.1f16,8.1f16,
                        9.1f16,10.1f16,11.1f16,12.1f16,
			13.1f16,14.1f16,15.1f16,16.1f16};
  ymmt1._m256h[0] = test_256h;
  ymmt2._m256h[0] = WRAP_RET (fun_test_returning___m256h)();
  if (memcmp (&ymmt1, &ymmt2, sizeof (ymmt2)) != 0)
    printf ("fail m256h\n"), failed++;

  if (failed)
    abort ();
}
