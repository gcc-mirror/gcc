/* Test generation of machhwu on 440.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mcpu=440" } */

/* { dg-final { scan-assembler "machhwu " } } */

unsigned int
f(unsigned int a, unsigned int b, unsigned int c)
{
  a += (b >> 16) * (c >> 16);
  return a;
}
