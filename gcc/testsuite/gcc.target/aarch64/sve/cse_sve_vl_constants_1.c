/* { dg-do compile } */
/* { dg-options "-O3 -moverride=tune=cse_sve_vl_constants" } */

void __attribute__((noinline, noclone))
vadd (int *dst, int *op1, int *op2, int count)
{
  for (int i = 0; i < count; ++i)
    dst[i] = op1[i] + op2[i];
}

/* { dg-final { scan-assembler-not {\tincw\tx[0-9]+} } } */

