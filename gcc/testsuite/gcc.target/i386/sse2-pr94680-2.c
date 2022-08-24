/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */
typedef double v2df __attribute__ ((vector_size (16)));
typedef long long v2di __attribute__((vector_size(16)));

v2df foo_v2df (v2df x)
{
  return __builtin_shuffle (x, (v2df) { 0, 0 }, (v2di) { 0, 2 });
}

/* { dg-final { scan-assembler "movq" } } */
/* { dg-final { scan-assembler-not "pxor" } } */

