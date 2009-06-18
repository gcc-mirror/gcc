/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-msse4" } */
#include <nmmintrin.h>
__m128i load (char *);
char *
foo (char *p1, char *p2,
     int bmsk, __m128i mask1, __m128i mask2)
{
  int len = 0;
  __m128i frag1, frag2;
  int  cmp_s;
  if( !p2[0]) return p1;
  if( !p1[0] ) return NULL;
  frag2 = load (p2); 
  frag1 = load (p1);
  frag2 = _mm_blendv_epi8(frag2, mask2, mask1);
  frag1 = _mm_blendv_epi8(frag1, mask1, mask2);
  cmp_s = _mm_cmpistrs(frag2, frag1, 0x0c);
  if( cmp_s )
    __asm("bsfl %[bmsk], %[len]" : [len] "=r" (len) : [bmsk] "r" (bmsk) );
  return p1 + len;
}
