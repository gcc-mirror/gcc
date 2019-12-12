/* Test generation of mulhhw on 440.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mdejagnu-cpu=440" } */

/* { dg-final { scan-assembler "mulhhw " } } */

int
f(int b, int c)
{
  int a = (b >> 16) * (c >> 16);
  return a;
}
