/* Test generation of macchwu. on 440.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mcpu=440" } */

/* { dg-final { scan-assembler "macchwu\\. " } } */

unsigned int
f(unsigned int a, unsigned int b, unsigned int c)
{
  a += (unsigned short)b * (c >> 16);
  if (!a)
    return 10;
  return a;
}
