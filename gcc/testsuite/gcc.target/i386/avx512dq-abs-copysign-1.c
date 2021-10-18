/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-Ofast -mavx512vl -mavx512dq" } */

void
f1 (float x)
{
  register float a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = __builtin_fabsf (a);
  asm volatile ("" : "+v" (a));
}

void
f2 (float x, float y)
{
  register float a __asm ("xmm16"), b __asm ("xmm17");
  a = x;
  b = y;
  asm volatile ("" : "+v" (a), "+v" (b));
  a = __builtin_copysignf (a, b);
  asm volatile ("" : "+v" (a));
}

void
f3 (float x)
{
  register float a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = -a;
  asm volatile ("" : "+v" (a));
}

void
f4 (double x)
{
  register double a __asm ("xmm18");
  a = x;
  asm volatile ("" : "+v" (a));
  a = __builtin_fabs (a);
  asm volatile ("" : "+v" (a));
}

void
f5 (double x, double y)
{
  register double a __asm ("xmm18"), b __asm ("xmm19");
  a = x;
  b = y;
  asm volatile ("" : "+v" (a), "+v" (b));
  a = __builtin_copysign (a, b);
  asm volatile ("" : "+v" (a));
}

void
f6 (double x)
{
  register double a __asm ("xmm18");
  a = x;
  asm volatile ("" : "+v" (a));
  a = -a;
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler "vandps\[^\n\r\]*xmm16" } } */
/* { dg-final { scan-assembler "vpternlogd\[^\n\r\]*xmm16" } } */
/* { dg-final { scan-assembler "vxorps\[^\n\r\]*xmm16" } } */
/* { dg-final { scan-assembler "vandpd\[^\n\r\]*xmm18" } } */
/* { dg-final { scan-assembler "vpternlogq\[^\n\r\]*xmm18" } } */
/* { dg-final { scan-assembler "vxorpd\[^\n\r\]*xmm18" } } */
