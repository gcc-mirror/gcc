/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl -mno-avx512dq -masm=att" } */

typedef int V1 __attribute__((vector_size (32)));
typedef long long V2 __attribute__((vector_size (32)));
typedef float V3 __attribute__((vector_size (32)));
typedef double V4 __attribute__((vector_size (32)));

void
f1 (V1 x, int y)
{
  register V1 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a[3] = y;
  asm volatile ("" : "+v" (a));
}

void
f2 (V1 x, int y)
{
  register V1 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a[6] = y;
  asm volatile ("" : "+v" (a));
}

void
f3 (V2 x, long long y)
{
  register V2 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a[1] = y;
  asm volatile ("" : "+v" (a));
}

void
f4 (V2 x, long long y)
{
  register V2 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a[3] = y;
  asm volatile ("" : "+v" (a));
}

void
f5 (V3 x, float y)
{
  register V3 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a[3] = y;
  asm volatile ("" : "+v" (a));
}

void
f6 (V3 x, float y)
{
  register V3 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a[6] = y;
  asm volatile ("" : "+v" (a));
}

void
f7 (V4 x, double y)
{
  register V4 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a[1] = y;
  asm volatile ("" : "+v" (a));
}

void
f8 (V4 x, double y)
{
  register V4 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a[3] = y;
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler-times "vinserti32x4\[^\n\r]*0x0\[^\n\r]*%ymm16" 2 } } */
/* { dg-final { scan-assembler-times "vinserti32x4\[^\n\r]*0x1\[^\n\r]*%ymm16" 2 } } */
/* { dg-final { scan-assembler-times "vinsertf32x4\[^\n\r]*0x0\[^\n\r]*%ymm16" 2 } } */
/* { dg-final { scan-assembler-times "vinsertf32x4\[^\n\r]*0x1\[^\n\r]*%ymm16" 2 } } */
/* { dg-final { scan-assembler-times "vextracti32x4\[^\n\r]*0x1\[^\n\r]*%\[yz]mm16" 2 } } */
/* { dg-final { scan-assembler-times "vextractf32x4\[^\n\r]*0x1\[^\n\r]*%\[yz]mm16" 2 } } */
/* { dg-final { scan-assembler-not "vinserti64x2" } } */
/* { dg-final { scan-assembler-not "vinsertf64x2" } } */
/* { dg-final { scan-assembler-not "vextracti64x2" } } */
/* { dg-final { scan-assembler-not "vextracti64x2" } } */
