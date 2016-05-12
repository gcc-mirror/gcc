/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-Ofast -mavx512vl" } */

void
f1 (__float128 x)
{
  register __float128 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = __builtin_fabsq (a);
  asm volatile ("" : "+v" (a));
}

void
f2 (__float128 x, __float128 y)
{
  register __float128 a __asm ("xmm16"), b __asm ("xmm17");
  a = x;
  b = y;
  asm volatile ("" : "+v" (a), "+v" (b));
  a = __builtin_copysignq (a, b);
  asm volatile ("" : "+v" (a));
}

void
f3 (__float128 x)
{
  register __float128 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = -a;
  asm volatile ("" : "+v" (a));
}

__int128_t
f4 (void)
{
  register __int128_t a __asm ("xmm16");
  register __int128_t __attribute__((vector_size (16))) b __asm ("xmm17");
  a = 1;
  asm volatile ("" : "+v" (a));
  b[0] = a;
  asm volatile ("" : "+v" (b));
  return b[0];
}

/* { dg-final { scan-assembler "vpandq\[^\n\r\]*xmm16" } } */
/* { dg-final { scan-assembler "vporq\[^\n\r\]*xmm16" } } */
/* { dg-final { scan-assembler "vpxorq\[^\n\r\]*xmm16" } } */
