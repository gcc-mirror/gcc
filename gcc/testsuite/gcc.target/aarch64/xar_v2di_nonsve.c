/* { dg-do compile } */
/* { dg-options "-O2" } */

#pragma GCC target "+sve2+sha3"

typedef unsigned long long __attribute__ ((vector_size (16))) v2di;

/* Both +sve2 and +sha3 have V2DImode XAR instructions, but we should
   prefer the Advanced SIMD one when both are available.  */
v2di
xar_v2di (v2di a, v2di b) {
  v2di c = a ^ b;
  return (c << 22) ^ (c >> 42);
}
/* { dg-final { scan-assembler {\txar\tv0.2d, v[0-9]+.2d, v[0-9]+.2d, 42} } } */

