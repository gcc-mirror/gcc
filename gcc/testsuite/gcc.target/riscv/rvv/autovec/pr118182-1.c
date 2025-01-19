/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -fno-vect-cost-model -O3 -mabi=lp64d" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

/* { dg-final { check-function-bodies "**" "" } } */

/*
** f1:
**   ...
**   vsetivli	zero,1,.*
**   ...
**   vfmv.s.f	.*
**   ...
**   vsetvli	zero,.*
**   ...
**   vfredosum.vs	.*
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
