#include <stdio.h>
#include "bf16-ymm-check.h"
#include "args.h"

struct FloatRegisters fregs;
struct IntegerRegisters iregs;
unsigned int num_fregs, num_iregs;

volatile __bf16 bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8,
		bf9, bf10,bf11,bf12,bf13,bf14,bf15,bf16;

__m256bf16
fun_test_returning___m256bf16 (void)
{
  volatile_var++;
  return (__m256bf16) { bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8,
			bf9, bf10,bf11,bf12,bf13,bf14,bf15,bf16};
}

__m256bf16 test_256bf16;

static void
do_test (void)
{
  unsigned failed = 0;
  YMM_T ymmt1, ymmt2;

  clear_struct_registers;
  test_256bf16 = (__m256bf16) { bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8,
				bf9, bf10,bf11,bf12,bf13,bf14,bf15,bf16};
  ymmt1._m256bf16[0] = test_256bf16;
  ymmt2._m256bf16[0] = WRAP_RET (fun_test_returning___m256bf16) ();
  if (memcmp (&ymmt1, &ymmt2, sizeof (ymmt2)) != 0)
    printf ("fail m256bf16\n"), failed++;

  if (failed)
    abort ();
}
