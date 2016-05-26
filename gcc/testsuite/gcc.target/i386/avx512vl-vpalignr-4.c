/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl -mno-avx512bw -masm=att" } */

typedef char V1 __attribute__((vector_size (16)));

void
f1 (V1 x)
{
  register V1 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = __builtin_shuffle (a, (V1) { 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5 });
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler-not "vpalignr\[^\n\r]*\\\$6\[^\n\r]*%xmm16\[^\n\r]*%xmm16\[^\n\r]*%xmm16" } } */

typedef short V2 __attribute__((vector_size (16)));

void
f2 (V2 x)
{
  register V2 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = __builtin_shuffle (a, (V2) { 5, 6, 7, 0, 1, 2, 3, 4 });
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler-not "vpalignr\[^\n\r]*\\\$10\[^\n\r]*%xmm16\[^\n\r]*%xmm16\[^\n\r]*%xmm16" } } */

typedef int V3 __attribute__((vector_size (16)));

void
f3 (V3 x)
{
  register V3 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = __builtin_shuffle (a, (V3) { 3, 0, 1, 2 });
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler-times "vpshufd\[^\n\r]*\\\$147\[^\n\r]*%xmm16\[^\n\r]*%xmm16" 1 } } */

typedef long long V4 __attribute__((vector_size (16)));

void
f4 (V4 x)
{
  register V4 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = __builtin_shuffle (a, (V4) { 1, 0 });
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler-not "vpalignr\[^\n\r]*\\\$8\[^\n\r]*%xmm16\[^\n\r]*%xmm16\[^\n\r]*%xmm16" } } */

typedef float V5 __attribute__((vector_size (16)));

void
f5 (V5 x)
{
  register V5 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = __builtin_shuffle (a, (V3) { 3, 0, 1, 2 });
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler-times "vpermilps\[^\n\r]*\\\$147\[^\n\r]*%xmm16\[^\n\r]*%xmm16" 1 } } */

typedef double V6 __attribute__((vector_size (16)));

void
f6 (V6 x)
{
  register V6 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = __builtin_shuffle (a, (V4) { 1, 0 });
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler-times "vpermilpd\[^\n\r]*\\\$1\[^\n\r]*%xmm16\[^\n\r]*%xmm16" 1 } } */
