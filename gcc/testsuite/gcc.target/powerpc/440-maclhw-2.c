/* Test generation of maclhw. on 440.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mcpu=440" } */

/* { dg-final { scan-assembler "maclhw\\. " } } */

int
f(int a, int b, int c)
{
  a += (short)b * (short)c;
  if (!a)
    return 10;
  return a;
}
