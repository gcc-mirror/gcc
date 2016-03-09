/* PR target/70086 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mtune=barcelona -mavx512vl" } */

float
foo (double *p)
{
  register float xmm16 __asm ("xmm16");
  xmm16 = *p;
  asm volatile ("" : "+v" (xmm16));
  return xmm16;
}

float
bar (double x)
{
  register float xmm16 __asm ("xmm16");
  xmm16 = x;
  asm volatile ("" : "+v" (xmm16));
  return xmm16;
}
