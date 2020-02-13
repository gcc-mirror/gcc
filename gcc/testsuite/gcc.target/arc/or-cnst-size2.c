/* Check if we optimize the immediate of a predicated instruction.  */
/* { dg-options "-Os -fif-conversion -fif-conversion2" } */

int a;
int foo (void)
{
  if ((a & 60) == 0)
    return a | 64;
}

/* { dg-final { scan-assembler "tst" } } */
/* { dg-final { scan-assembler "bset" } } */
