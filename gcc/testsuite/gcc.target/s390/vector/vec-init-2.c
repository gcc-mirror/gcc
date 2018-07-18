/* Check that the vec_init expander does its job.  */

/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z13" } */
/* { dg-require-effective-target int128 } */




typedef __attribute__((vector_size(16))) double v2df;
typedef __attribute__((vector_size(16))) long long v2di;

typedef __attribute__((vector_size(16))) long double v1tf;
typedef __attribute__((vector_size(16))) __int128 v1ti;

v1tf gld;

v1tf
f (long double a)
{
  return (v1tf){ a };
}

v1ti
g (__int128 a)
{
  return (v1ti){ a };
}
/* { dg-final { scan-assembler-times "vl\t" 2 } } */

v1tf
h ()
{
  long double a;
  asm volatile ("" : "=f" (a));
  return (v1tf){ a };
}

/* { dg-final { scan-assembler-times "vmrhg\t" 1 } } */

v1ti
i ()
{
  __int128 a;
  asm volatile ("" : "=d" (a));
  return (v1ti){ a };
}
/* { dg-final { scan-assembler-times "vlvgp\t" 1 } } */
