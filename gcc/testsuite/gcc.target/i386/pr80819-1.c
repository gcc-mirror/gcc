/* PR target/80819 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse4 -mno-avx -mtune=haswell -masm=att" } */

typedef unsigned long long V __attribute__((vector_size (16)));

V
foo (unsigned long long x, unsigned long long y)
{
  return (V) { x, y };
}

/* { dg-final { scan-assembler-not "movq\[ \t]*%rsi, \[-0-9]*\\(" } } */
