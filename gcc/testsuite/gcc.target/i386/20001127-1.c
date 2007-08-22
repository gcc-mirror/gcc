/* { dg-do compile } */
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
