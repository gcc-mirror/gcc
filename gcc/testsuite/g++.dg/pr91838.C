/* { dg-do compile } */
/* { dg-additional-options "-O2" } */
/* { dg-skip-if "" { *-*-* } {-std=c++98} } */

using T = unsigned char; // or ushort, or uint
using V [[gnu::vector_size(8)]] = T;
V f(V x) {
  return x >> 8 * sizeof(T);
}

/* { dg-final { scan-assembler {pxor\s+%xmm0,\s+%xmm0} { target x86_64-*-* } } } */
