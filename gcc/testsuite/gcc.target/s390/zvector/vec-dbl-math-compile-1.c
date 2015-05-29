/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13 -mzvector --save-temps" } */

/* { dg-final { scan-assembler-times "vfcedb\t" 1 } } */
/* { dg-final { scan-assembler-times "vfchdb\t" 2 } } */
/* { dg-final { scan-assembler-times "vfchedb\t" 2 } } */

/* { dg-final { scan-assembler-times "vfcedbs\t" 2 } } */
/* { dg-final { scan-assembler-times "vfchdbs\t" 2 } } */


#include <vecintrin.h>

vector bool long long
cmpeq (vector double a, vector double b)
{
  return vec_cmpeq (a, b); /* vfcedb */
}

vector bool long long
cmpgt (vector double a, vector double b)
{
  return vec_cmpgt (a, b); /* vfchdb */
}

vector bool long long
cmpge (vector double a, vector double b)
{
  return vec_cmpge (a, b); /* vfchedb */
}

vector bool long long
cmplt (vector double a, vector double b)
{
  return vec_cmplt (a, b); /* vfchdb */
}

vector bool long long
cmple (vector double a, vector double b)
{
  return vec_cmple (a, b); /* vfchedb */
}

int
all_eq (vector double a, vector double b)
{
  return vec_all_eq (a, b);
}

int
any_eq (vector double a, vector double b)
{
  return vec_any_eq (a, b);
}

int
all_lt (vector double a, vector double b)
{
  return vec_all_lt (a, b);
}

int
any_lt (vector double a, vector double b)
{
  return vec_any_lt (a, b);
}
