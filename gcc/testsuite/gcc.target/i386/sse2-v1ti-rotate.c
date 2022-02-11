/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

typedef unsigned __int128 uv1ti __attribute__ ((__vector_size__ (16)));

uv1ti rotr(uv1ti x, unsigned int i) { return (x >> i) | (x << (128-i)); }
uv1ti rotl(uv1ti x, unsigned int i) { return (x << i) | (x >> (128-i)); }

/* { dg-final { scan-assembler-not "shrq" } } */
/* { dg-final { scan-assembler-not "salq" } } */
