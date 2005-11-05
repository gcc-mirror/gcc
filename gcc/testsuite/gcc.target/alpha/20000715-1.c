/* { dg-do compile { target alpha*-*-* } } */
/* { dg-options "-O2 -mieee" } */

float foo(unsigned char n)
{
  float r = 10 * n;
  asm volatile("" : : : "memory");
  return r;
}
