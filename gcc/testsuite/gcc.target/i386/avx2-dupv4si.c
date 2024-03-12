/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler-times "vpbroadcastd" 2 } } */

typedef int __attribute__ ((vector_size (16))) v4si;

v4si bcst_reg (int i)
{
  register int x asm ("xmm7") = i;

  asm ("" : "+v" (x));
  return (v4si) {x, x, x, x};
}

v4si bcst_mem (const int *i)
{
  return (v4si) {*i, *i, *i, *i};
}
