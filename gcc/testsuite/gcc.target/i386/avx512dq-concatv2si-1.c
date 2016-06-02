/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl -mavx512dq -masm=att" } */

typedef int V __attribute__((vector_size (8)));

void
f1 (int x, int y)
{
  register int a __asm ("xmm16");
  register int b __asm ("xmm17");
  register V c __asm ("xmm3");
  a = x;
  b = y;
  asm volatile ("" : "+v" (a), "+v" (b));
  c = (V) { a, b };
  asm volatile ("" : "+v" (c));
}

/* { dg-final { scan-assembler "vpunpckldq\[^\n\r]*%xmm17\[^\n\r]*%xmm16\[^\n\r]*%xmm3" } } */

void
f2 (int x, int y)
{
  register int a __asm ("xmm16");
  register V c __asm ("xmm3");
  a = x;
  asm volatile ("" : "+v" (a));
  c = (V) { a, y };
  asm volatile ("" : "+v" (c));
}

void
f3 (int x, int *y)
{
  register int a __asm ("xmm16");
  register V c __asm ("xmm3");
  a = x;
  asm volatile ("" : "+v" (a));
  c = (V) { a, *y };
  asm volatile ("" : "+v" (c));
}

/* { dg-final { scan-assembler-times "vpinsrd\[^\n\r]*\\\$1\[^\n\r]*%xmm16\[^\n\r]*%xmm3" 2 } } */
