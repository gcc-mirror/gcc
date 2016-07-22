/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl -masm=att" } */

typedef char V1 __attribute__((vector_size (32)));
typedef short V2 __attribute__((vector_size (32)));

void
f1 (V1 x, char y)
{
  register V1 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a[7] = y;
  asm volatile ("" : "+v" (a));
}

void
f2 (V1 x, char y)
{
  register V1 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a[28] = y;
  asm volatile ("" : "+v" (a));
}

void
f3 (V2 x, short y)
{
  register V2 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a[3] = y;
  asm volatile ("" : "+v" (a));
}

void
f4 (V2 x, short y)
{
  register V2 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a[14] = y;
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler-times "vinserti32x4\[^\n\r]*0x0\[^\n\r]*%ymm16" 2 } } */
/* { dg-final { scan-assembler-times "vinserti32x4\[^\n\r]*0x1\[^\n\r]*%ymm16" 2 } } */
/* { dg-final { scan-assembler-times "vextracti32x4\[^\n\r]*0x1\[^\n\r]*%\[yz]mm16" 2 } } */
