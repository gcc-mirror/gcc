/* { dg-do compile { target { fpic && ilp32 } } } */
/* { dg-options "-O2 -fpic" } */
/* { dg-final { scan-assembler-not "lis.*@ha" } } */

union U { double x; int i[2]; };

double
foo (double x)
{
  union U v;
  v.i[0] = 0x7ff00000;
  v.i[1] = 0;
  return x / v.x;
}
