/* PR target/68483 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-sse3" } */

typedef int V __attribute__((vector_size (16)));

void
foo (V *a, V *b)
{
  V c = { 0, 0, 0, 0 };
  V d = { 1, 2, 3, 4 };
  *a = __builtin_shuffle (*b, c, d);
}

/* { dg-final { scan-assembler "psrldq\[^\n\r]*(4,|, 4)" } } */
