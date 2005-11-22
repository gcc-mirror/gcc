/* Test generation of machhw on 440.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mcpu=440" } */

/* { dg-final { scan-assembler "machhw " } } */

int
f(int a, int b, int c)
{
  a += (b >> 16) * (c >> 16);
  return a;
}
