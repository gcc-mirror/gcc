/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

typedef short __v8hi __attribute__ ((__vector_size__ (16)));
typedef long long __m128i __attribute__ ((__vector_size__ (16)));
__m128i Set_AC4R_SETUP_I( const short *val ) {
  short D2073 = *val;
  short D2076 = *(val + 2);
  short D2079 = *(val + 4);
  __v8hi D2094 = {D2073, D2076, D2079, 0, D2073, D2076, D2079, 0};
  return (__m128i)D2094;
}
