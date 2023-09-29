/* { dg-do compile } */
/* { dg-options "-march=rv32g_zfa -mabi=ilp32 -O0" } */

double foo(long long a)
{
  return (double)(a + 3);
}

/* { dg-final { scan-assembler-times "fmvp.d.x" 1 } } */
/* { dg-final { scan-assembler-times "fmvh.x.d" 1 } } */
