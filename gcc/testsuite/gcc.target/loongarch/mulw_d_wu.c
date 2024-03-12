/* { dg-do compile } */
/* { dg-options "-O2 -mabi=lp64d" } */
/* { dg-final { scan-assembler "mulw.d.wu" } } */

__attribute__((noipa, noinline)) unsigned long
f(unsigned long a, unsigned long b)
{
  return (unsigned long)(unsigned int)a * (unsigned long)(unsigned int)b;
}
