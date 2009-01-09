/* Test generation of nmacchw on 405.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mcpu=405" } */
/* { dg-skip-if "other options override -mcpu=405" { ! powerpc_405_nocache } { "*" } { "" } } */

/* { dg-final { scan-assembler "nmacchw " } } */

int
f(int a, int b, int c)
{
  a -= (short)b * (c >> 16);
  return a;
}
