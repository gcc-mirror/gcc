/* PR target/51492 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msse2" } */

#define SIZE 65536
#define WSIZE 64
unsigned short head[SIZE] __attribute__((aligned(64)));

void
f(void)
{
  for (unsigned n = 0; n < SIZE; ++n) {
    unsigned short m = head[n];
    head[n] = (unsigned short)(m >= WSIZE ? m-WSIZE : 0);
  }
}

/* { dg-final { scan-assembler "psubusw" } } */
/* { dg-final { scan-assembler-not "paddw" } } */
