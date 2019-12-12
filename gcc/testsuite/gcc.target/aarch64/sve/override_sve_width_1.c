/* { dg-do compile } */
/* { dg-options "-O3 -moverride=sve_width=512" } */

void __attribute__((noinline, noclone))
vadd (int *dst, int *op1, int *op2, int count)
{
  for (int i = 0; i < count; ++i)
    dst[i] = op1[i] + op2[i];
}
