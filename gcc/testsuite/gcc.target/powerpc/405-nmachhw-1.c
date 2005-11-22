/* Test generation of nmachhw on 405.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mcpu=405" } */

/* { dg-final { scan-assembler "nmachhw " } } */

int
f(int a, int b, int c)
{
  a -= (b >> 16) * (c >> 16);
  return a;
}
