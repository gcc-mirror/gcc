#include <stdio.h>
#include "bf16-zmm-check.h"
#include "args.h"

struct FloatRegisters fregs;
struct IntegerRegisters iregs;
unsigned int num_fregs, num_iregs;

volatile __bf16 bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8,
		bf9, bf10,bf11,bf12,bf13,bf14,bf15,bf16,
		bf17,bf18,bf19,bf20,bf21,bf22,bf23,bf24,
		bf25,bf26,bf27,bf28,bf29,bf30,bf31,bf32;

__m512bf16
fun_test_returning___m512bf16 (void)
{
  volatile_var++;
  return (__m512bf16) { bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8,
			bf9, bf10,bf11,bf12,bf13,bf14,bf15,bf16,
			bf17,bf18,bf19,bf20,bf21,bf22,bf23,bf24,
			bf25,bf26,bf27,bf28,bf29,bf30,bf31,bf32 };
}

__m512bf16 test_512bf16;

static void
do_test (void)
{
  unsigned failed = 0;
  ZMM_T zmmt1, zmmt2;

  clear_struct_registers;
  test_512bf16 = (__m512bf16) { bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8,
				bf9, bf10,bf11,bf12,bf13,bf14,bf15,bf16,
				bf17,bf18,bf19,bf20,bf21,bf22,bf23,bf24,
				bf25,bf26,bf27,bf28,bf29,bf30,bf31,bf32 };
  zmmt1._m512bf16[0] = test_512bf16;
  zmmt2._m512bf16[0] = WRAP_RET (fun_test_returning___m512bf16)();
  if (memcmp (&zmmt1, &zmmt2, sizeof (zmmt2)) != 0)
    printf ("fail m512bf16\n"), failed++;

  if (failed)
    abort ();
}
