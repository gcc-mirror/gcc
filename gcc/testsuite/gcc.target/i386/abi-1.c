/* Make certain that we pass V2DF in the correct register for SSE1.  */
/* { dg-do compile } */
/* { dg-options "-O1 -msse -mno-sse2" } */

typedef double v2df __attribute__((vector_size (16)));
v2df foo (void) { return (v2df){ 1.0, 2.0 }; }

/* { dg-final { scan-assembler "xmm0" } } */
