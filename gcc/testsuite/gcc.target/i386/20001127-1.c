/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2" } */

extern inline float bar (float x)
{
  register long double value;
  asm volatile ("frndint" : "=t" (value) : "0" (x));
  return value;
}

float a;

float foo (float b)
{
  return a + bar (b);
}
