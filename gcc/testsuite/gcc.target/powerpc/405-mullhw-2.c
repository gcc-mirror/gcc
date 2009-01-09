/* Test generation of mullhw. on 405.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mcpu=405" } */
/* { dg-skip-if "other options override -mcpu=405" { ! powerpc_405_nocache } { "*" } { "" } } */

/* { dg-final { scan-assembler "mullhw\\. " } } */

int
f(int b, int c)
{
  int a = (short)b * (short)c;
  if (!a)
    return 10;
  return a;
}
