/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl -mavx512dq -masm=att -mtune=haswell" } */

typedef long long V __attribute__((vector_size (16)));

void
f1 (long long x, long long y)
{
  register long long a __asm ("xmm16");
  register V c __asm ("xmm17");
  a = x;
  asm volatile ("" : "+v" (a));
  c = (V) { a, y };
  asm volatile ("" : "+v" (c));
}

/* { dg-final { scan-assembler "vpinsrq\[^\n\r]*\\\$1\[^\n\r]*%rsi\[^\n\r]*%xmm16\[^\n\r]*%xmm17" } } */

void
f2 (long long x, long long *y)
{
  register long long a __asm ("xmm18");
  register V c __asm ("xmm19");
  a = x;
  asm volatile ("" : "+v" (a));
  c = (V) { a, *y };
  asm volatile ("" : "+v" (c));
}

/* { dg-final { scan-assembler "vpinsrq\[^\n\r]*\\\$1\[^\n\r]*%\[re]si\[^\n\r]*%xmm18\[^\n\r]*%xmm19" } } */

void
f3 (long long x)
{
  register V a __asm ("xmm20");
  a = (V) { x, 0 };
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler "vmov\[dq]\[^\n\r]*%rdi\[^\n\r]*%xmm20" } } */

void
f4 (long long *x)
{
  register V a __asm ("xmm21");
  a = (V) { *x, 0 };
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler "vmovq\[^\n\r]*%\[re]di\[^\n\r]*%xmm21" } } */

void
f5 (long long x)
{
  register long long a __asm ("xmm22");
  register V c __asm ("xmm23");
  a = x;
  asm volatile ("" : "+v" (a));
  c = (V) { a, 0 };
  asm volatile ("" : "+v" (c));
}

/* { dg-final { scan-assembler "vmovq\[^\n\r]*%xmm22\[^\n\r]*%xmm23" } } */

void
f6 (long long x, long long y)
{
  register long long a __asm ("xmm24");
  register long long b __asm ("xmm25");
  register V c __asm ("xmm26");
  a = x;
  b = y;
  asm volatile ("" : "+v" (a), "+v" (b));
  c = (V) { a, b };
  asm volatile ("" : "+v" (c));
}

/* { dg-final { scan-assembler "vpunpcklqdq\[^\n\r]*%xmm25\[^\n\r]*%xmm24\[^\n\r]*%xmm26" } } */
