/* Test generation of mulchw on 440.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mcpu=440" } */

/* { dg-final { scan-assembler "mulchw " } } */

int
f(int b, int c)
{
  int a = (short)b * (c >> 16);
  return a;
}
