#include <stdio.h>
#include "avx512f-check.h"
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

__m512 test_512;

static void
avx512f_test (void)
{
  unsigned failed = 0;
  ZMM_T zmmt1, zmmt2;

  clear_struct_registers;
  test_512 = (__m512){73,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  zmmt1._m512[0] = test_512;
  zmmt2._m512[0] = WRAP_RET (fun_test_returning___m512)();
  if (memcmp (&zmmt1, &zmmt2, sizeof (zmmt2)) != 0)
    printf ("fail m512\n"), failed++;
  if (failed)
    abort ();
}
