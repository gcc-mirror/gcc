/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-msse4" } */
#include <nmmintrin.h>
__m128i load (char *);
char *
foo (const unsigned char *s1, const unsigned char *s2,
     int bmsk, __m128i frag2)
{
  int len = 0;
  char *p1 = (char  *) s1;
  char *p2 = (char  *) s2;
  __m128i frag1, fruc1, fruc2, mask1, mask2;
  int cmp_c, cmp_s;
  if( !p2[0]) return (char *) s1;
  if( !p1[0] ) return NULL;
  if( p2[1]) frag2 = load (p2); 
  frag1 = load (p1);
  fruc1 = _mm_loadu_si128 ((__m128i *) s1);
  fruc2 = _mm_loadu_si128 ((__m128i *) s2);
  mask1 = _mm_cmpistrm(fruc1, frag2, 0x44);
  mask2 = _mm_cmpistrm(fruc2, frag1, 0x14);
  frag2 = _mm_blendv_epi8(frag2, mask1, mask2);
  frag1 = _mm_blendv_epi8(frag1, mask2, mask1);
  cmp_c = _mm_cmpistrc(frag2, frag1, 0x0c);
  cmp_s = _mm_cmpistrs(frag2, frag1, 0x0c);
  if( cmp_s  & cmp_c  )
    __asm("bsfl %[bmsk], %[len]" : [len] "=r" (len) : [bmsk] "r" (bmsk) );
  return p2 + len;
}
