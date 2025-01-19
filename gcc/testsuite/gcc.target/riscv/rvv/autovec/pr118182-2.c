/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -fno-vect-cost-model -O3 -mabi=lp64d -ffast-math" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

/* { dg-final { check-function-bodies "**" "" } } */

/*
** f1:
**   ...
**   vsetvli	[ast][0-9]+,zero,.*
**   ...
**   vmv.s.x	.*
**   ...
**   vfredusum.vs	.*
**   ...
**   vfmv.f.s	.*
**   ...
*/

float f1(float *arr, int n)
{
  float sum = 0;
  for (int i = 0; i < n; i++)
    sum += arr[i];
  return sum;
}
/* { dg-final { scan-assembler-not {\tvsetivli\tzero,1,.*} } } */
