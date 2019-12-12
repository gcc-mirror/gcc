/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -fdump-tree-optimized" } */

typedef double __v2df __attribute__ ((__vector_size__ (16)));
typedef long long __v2di __attribute__ ((__vector_size__ (16)));

__v2df
_mm_add_sd_A (__v2df x, __v2df y)
{
  double tem = x[0] + y[0];
  return __builtin_shuffle ( x, (__v2df) { tem, tem }, (__v2di) { 2, 1 } );
}

__v2df
_mm_add_sd_B (__v2df x, __v2df y)
{
  __v2df z = { (x[0] + y[0]), x[1] };
  return z;
}

/* { dg-final { scan-tree-dump-times "BIT_INSERT_EXPR" 2 "optimized" } } */
/* { dg-final { scan-assembler-not "unpck" } } */
