/* Test generation of mullhw. on 440.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mcpu=440" } */

/* { dg-final { scan-assembler "mullhw\\. " } } */

int
f(int b, int c)
{
  int a = (short)b * (short)c;
  if (!a)
    return 10;
  return a;
}
