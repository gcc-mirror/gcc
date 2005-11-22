/* Test generation of mulhhw. on 405.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mcpu=405" } */

/* { dg-final { scan-assembler "mulhhw\\. " } } */

int
f(int b, int c)
{
  int a = (b >> 16) * (c >> 16);
  if (!a)
    return 10;
  return a;
}
