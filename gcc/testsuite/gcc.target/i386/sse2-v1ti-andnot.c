/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse2" } */

typedef __int128 v1ti __attribute__ ((__vector_size__ (16)));

v1ti andnot1(v1ti x, v1ti y) { return ~x & y; }
v1ti andnot2(v1ti x, v1ti y) { return x & ~y; }

/* { dg-final { scan-assembler-times "pandn" 2 } } */
/* { dg-final { scan-assembler-not "pcmpeqd" } } */
/* { dg-final { scan-assembler-not "pxor" } } */
