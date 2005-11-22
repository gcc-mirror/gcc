/* Test generation of mulchwu. on 405.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mcpu=405" } */

/* { dg-final { scan-assembler "mulchwu\\. " } } */

int
f(unsigned int b, unsigned int c)
{
  unsigned int a = (unsigned short)b * (c >> 16);
  if (!a)
    return 10;
  return a;
}
