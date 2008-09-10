/* { dg-do compile } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

typedef char __v16qi __attribute__ ((__vector_size__ (16)));
typedef long long __m128i __attribute__ ((__vector_size__ (16)));
__m128i Set_AC4R_SETUP_I( const char *val ) {
  char D2073 = *val;
  char D2074 = *(val + 1);
  char D2075 = *(val + 2);
  char D2076 = *(val + 3);
  char D2077 = *(val + 4);
  char D2078 = *(val + 5);
  char D2079 = *(val + 6);
  __v16qi D2094 = {D2073, D2074, D2075, D2076,  D2077, D2078, D2079, 0,
  D2073, D2074, D2075, D2076,  D2077, D2078, D2079, 0};
  return (__m128i)D2094;
}
