/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -mno-vsx -O1" } */
/* { dg-require-effective-target powerpc_altivec } */
/* { dg-final { scan-assembler-times "lvx %?v?2,%?r?3" 1 } } */
/* { dg-final { scan-assembler-times "stvx %?v?2,%?r?3" 1 } } */

/* PR target/71733.  */
typedef __attribute__ ((altivec(vector__), aligned(16))) unsigned char vec_t;

vec_t
f1 (vec_t *dst)
{
  return dst[1];
}

void
f2 (vec_t *dst, vec_t src)
{
  dst[1] = src;
}
