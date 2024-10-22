/* { dg-do compile } */
/* { dg-options "-O2" } */

#pragma GCC target "+sve2+nosha3"

typedef char __attribute__ ((vector_size (16))) v16qi;
typedef unsigned short __attribute__ ((vector_size (16))) v8hi;
typedef unsigned int __attribute__ ((vector_size (16))) v4si;
typedef unsigned long long __attribute__ ((vector_size (16))) v2di;

v16qi
xar_v16qi (v16qi a, v16qi b) {
  v16qi c = a ^ b;
  return (c << 2) ^ (c >> 6);
}
/* { dg-final { scan-assembler {\txar\tz0.b, z[0-9]+.b, z[0-9]+.b, #6} } } */

v8hi
xar_v8hi (v8hi a, v8hi b) {
  v8hi c = a ^ b;
  return (c << 13) ^ (c >> 3);
}
/* { dg-final { scan-assembler {\txar\tz0.h, z[0-9]+.h, z[0-9]+.h, #3} } } */

v4si
xar_v4si (v4si a, v4si b) {
  v4si c = a ^ b;
  return (c << 9) ^ (c >> 23);
}
/* { dg-final { scan-assembler {\txar\tz0.s, z[0-9]+.s, z[0-9]+.s, #23} } } */

/* When +sha3 for Advanced SIMD is not available we should still use the
   SVE2 form of XAR.  */
v2di
xar_v2di (v2di a, v2di b) {
  v2di c = a ^ b;
  return (c << 22) ^ (c >> 42);
}
/* { dg-final { scan-assembler {\txar\tz0.d, z[0-9]+.d, z[0-9]+.d, #42} } } */
