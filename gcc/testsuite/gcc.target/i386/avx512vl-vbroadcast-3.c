/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl -masm=att" } */

typedef float V1 __attribute__((vector_size (16)));
typedef float V2 __attribute__((vector_size (32)));
typedef int V4 __attribute__((vector_size (16)));
typedef int V5 __attribute__((vector_size (32)));

void
f1 (V1 x)
{
  register V1 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = __builtin_shuffle (a, (V4) { 0, 0, 0, 0 });
  asm volatile ("" : "+v" (a));
}

void
f2 (V1 x)
{
  register V1 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = __builtin_shuffle (a, (V4) { 1, 1, 1, 1 });
  asm volatile ("" : "+v" (a));
}

void
f3 (V1 x)
{
  register V1 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = __builtin_shuffle (a, (V4) { 2, 2, 2, 2 });
  asm volatile ("" : "+v" (a));
}

void
f4 (V1 x)
{
  register V1 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = __builtin_shuffle (a, (V4) { 3, 3, 3, 3 });
  asm volatile ("" : "+v" (a));
}

void
f5 (V1 *x)
{
  register V1 a __asm ("xmm16");
  a = __builtin_shuffle (*x, (V4) { 0, 0, 0, 0 });
  asm volatile ("" : "+v" (a));
}

void
f6 (V1 *x)
{
  register V1 a __asm ("xmm16");
  a = __builtin_shuffle (*x, (V4) { 1, 1, 1, 1 });
  asm volatile ("" : "+v" (a));
}

void
f7 (V1 *x)
{
  register V1 a __asm ("xmm16");
  a = __builtin_shuffle (*x, (V4) { 2, 2, 2, 2 });
  asm volatile ("" : "+v" (a));
}

void
f8 (V1 *x)
{
  register V1 a __asm ("xmm16");
  a = __builtin_shuffle (*x, (V4) { 3, 3, 3, 3 });
  asm volatile ("" : "+v" (a));
}

void
f9 (V2 x)
{
  register V2 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = __builtin_shuffle (a, (V5) { 0, 0, 0, 0, 0, 0, 0, 0 });
  asm volatile ("" : "+v" (a));
}

void
f10 (V2 x)
{
  register V2 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = __builtin_shuffle (a, (V5) { 1, 1, 1, 1, 1, 1, 1, 1 });
  asm volatile ("" : "+v" (a));
}

void
f11 (V2 x)
{
  register V2 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = __builtin_shuffle (a, (V5) { 4, 4, 4, 4, 4, 4, 4, 4 });
  asm volatile ("" : "+v" (a));
}

void
f12 (V2 x)
{
  register V2 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = __builtin_shuffle (a, (V5) { 5, 5, 5, 5, 5, 5, 5, 5 });
  asm volatile ("" : "+v" (a));
}

void
f13 (V2 *x)
{
  register V2 a __asm ("xmm16");
  a = __builtin_shuffle (*x, (V5) { 0, 0, 0, 0, 0, 0, 0, 0 });
  asm volatile ("" : "+v" (a));
}

void
f14 (V2 *x)
{
  register V2 a __asm ("xmm16");
  a = __builtin_shuffle (*x, (V5) { 1, 1, 1, 1, 1, 1, 1, 1 });
  asm volatile ("" : "+v" (a));
}

void
f15 (V2 *x)
{
  register V2 a __asm ("xmm16");
  a = __builtin_shuffle (*x, (V5) { 4, 4, 4, 4, 4, 4, 4, 4 });
  asm volatile ("" : "+v" (a));
}

void
f16 (V2 *x)
{
  register V2 a __asm ("xmm16");
  a = __builtin_shuffle (*x, (V5) { 5, 5, 5, 5, 5, 5, 5, 5 });
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler-times "vbroadcastss\[^\n\r]*%rdi\[^\n\r]*%xmm16" 4 } } */
/* { dg-final { scan-assembler-times "vbroadcastss\[^\n\r]*%xmm16\[^\n\r]*%ymm16" 3 } } */
/* { dg-final { scan-assembler-times "vbroadcastss\[^\n\r]*%rdi\[^\n\r]*%ymm16" 3 } } */
/* { dg-final { scan-assembler-times "vpermilps\[^\n\r]*\\\$0\[^\n\r]*%xmm16\[^\n\r]*%xmm16" 1 } } */
/* { dg-final { scan-assembler-times "vpermilps\[^\n\r]*\\\$85\[^\n\r]*%xmm16\[^\n\r]*%xmm16" 1 } } */
/* { dg-final { scan-assembler-times "vpermilps\[^\n\r]*\\\$170\[^\n\r]*%xmm16\[^\n\r]*%xmm16" 1 } } */
/* { dg-final { scan-assembler-times "vpermilps\[^\n\r]*\\\$255\[^\n\r]*%xmm16\[^\n\r]*%xmm16" 1 } } */
/* { dg-final { scan-assembler-times "vpermilps\[^\n\r]*\\\$0\[^\n\r]*%ymm16\[^\n\r]*%ymm16" 1 } } */
/* { dg-final { scan-assembler-times "vpermilps\[^\n\r]*\\\$85\[^\n\r]*%ymm16\[^\n\r]*%ymm16" 2 } } */
/* { dg-final { scan-assembler-times "vshuff32x4\[^\n\r]*\\\$3\[^\n\r]*%ymm16\[^\n\r]*%ymm16\[^\n\r]*%ymm16" 2 } } */
