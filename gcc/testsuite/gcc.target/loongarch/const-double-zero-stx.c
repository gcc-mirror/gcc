/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times {stx\..\t\$r0} 2 } } */

extern float arr_f[];
extern double arr_d[];

void
test_f (int base, int index)
{
  arr_f[base + index] = 0.0;
}

void
test_d (int base, int index)
{
  arr_d[base + index] = 0.0;
}
