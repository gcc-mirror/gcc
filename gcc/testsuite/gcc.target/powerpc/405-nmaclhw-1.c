/* Test generation of nmaclhw on 405.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mdejagnu-cpu=405" } */

/* { dg-final { scan-assembler "nmaclhw " } } */

int
f(int a, int b, int c)
{
  a -= (short)b * (short)c;
  return a;
}
