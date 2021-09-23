#include <stdio.h>
#include "avx512fp16-zmm-check.h"
#include "args.h"

struct IntegerRegisters iregs;
struct FloatRegisters fregs;
unsigned int num_iregs, num_fregs;

__m512
fun_test_returning___m512 (void)
{
  volatile_var++;
  return (__m512){73,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
}

__m512h
fun_test_returning___m512h (void)
{
  volatile_var++;
  return (__m512h){ 1.1f16, 2.2f16, 3.3f16, 4.4f16,
                    5.5f16, 6.6f16, 7.7f16, 8.8f16,
                    9.9f16,  10.10f16,   11.11f16, 12.12f16,
                    13.13f16, 14.14f16,  15.15f16, 16.16f16,
                    17.17f16, 18.18f16,  19.19f16, 20.20f16,
                    21.21f16, 22.22f16,  23.23f16, 24.24f16,
                    25.25f16, 26.26f16,  27.27f16, 28.28f16,
                    29.29f16, 30.30f16,  31.31f16, 32.32f16};
}

__m512 test_512;
__m512h test_512h;

static void
do_test (void)
{
  unsigned failed = 0;
  ZMM_T zmmt1, zmmt2;

  clear_struct_registers;
  test_512 = (__m512){73,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  zmmt1._m512[0] = test_512;
  zmmt2._m512[0] = WRAP_RET (fun_test_returning___m512)();
  if (memcmp (&zmmt1, &zmmt2, sizeof (zmmt2)) != 0)
    printf ("fail m512\n"), failed++;

  clear_struct_registers;
  test_512h = (__m512h){ 1.1f16, 2.2f16, 3.3f16, 4.4f16,
                         5.5f16, 6.6f16, 7.7f16, 8.8f16,
                         9.9f16,  10.10f16,   11.11f16, 12.12f16,
                         13.13f16, 14.14f16,  15.15f16, 16.16f16,
                         17.17f16, 18.18f16,  19.19f16, 20.20f16,
                         21.21f16, 22.22f16,  23.23f16, 24.24f16,
                         25.25f16, 26.26f16,  27.27f16, 28.28f16,
                         29.29f16, 30.30f16,  31.31f16, 32.32f16};
  zmmt1._m512h[0] = test_512h;
  zmmt2._m512h[0] = WRAP_RET (fun_test_returning___m512h)();
  if (memcmp (&zmmt1, &zmmt2, sizeof (zmmt2)) != 0)
    printf ("fail m512h\n"), failed++;

  if (failed)
    abort ();
}
