/* Test generation of macchw on 405.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mcpu=405" } */

/* { dg-final { scan-assembler "macchw " } } */

int
f(int a, int b, int c)
{
  a += (short)b * (c >> 16);
  return a;
}
