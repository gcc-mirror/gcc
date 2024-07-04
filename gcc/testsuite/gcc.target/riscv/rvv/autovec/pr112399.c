/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-vect-cost-model -ffast-math -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */


/*
** foo:
**   ...
**   vsetvli\s*[a-x0-9]+,\s*[a-x0-9]+,\s*e64,\s*m1,\s*tu,\s*m[au]
**   slli\s*[a-x0-9]+,\s*[a-x0-9]+,\s*3
**   vle64\.v\s*v[0-9]+,\s*0\([a-x0-9]+\)
**   vle64\.v\s*v[0-9]+,\s*0\([a-x0-9]+\)
**   vfmul\.vv\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+
**   vle64\.v\s*\s*v[0-9]+,\s*0\([a-x0-9]+\)
**   vfmacc\.vv\s*\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+
**   add\s*[a-x0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+
**   add\s*[a-x0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+
**   add\s*[a-x0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+
**   sub\s*[a-x0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+
**   ...
*/

double
foo (double *__restrict a, double *__restrict b, double *__restrict c, int n)
{
  double result = 0;
  for (int i = 0; i < n; i++)
    result += a[i] * b[i] * c[i];
  return result;
}

