/* { dg-do compile { target c++11 } } */
/* { dg-additional-options "-O2 -Wno-psabi -w" } */
/* { dg-additional-options "-masm=att" { target i?86-*-* x86_64-*-* } } */

using T = unsigned char; // or ushort
using V [[gnu::vector_size(8)]] = T;
V f(V x) {
  return x >> 8 * sizeof(T);
}

/* { dg-final { scan-assembler {pxor\s+%xmm0,\s+%xmm0} { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
