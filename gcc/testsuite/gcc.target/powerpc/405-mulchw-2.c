/* Test generation of mulchw. on 405.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mdejagnu-cpu=405" } */

/* { dg-final { scan-assembler "mulchw\\. " } } */

int
f(int b, int c)
{
  int a = (short)b * (c >> 16);
  if (!a)
    return 10;
  return a;
}
