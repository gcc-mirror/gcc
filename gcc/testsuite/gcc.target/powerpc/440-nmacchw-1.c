/* Test generation of nmacchw on 440.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mdejagnu-cpu=440" } */

/* { dg-final { scan-assembler "nmacchw " } } */

int
f(int a, int b, int c)
{
  a -= (short)b * (c >> 16);
  return a;
}
