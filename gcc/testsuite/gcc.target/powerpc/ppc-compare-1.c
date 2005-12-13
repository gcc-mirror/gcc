/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-not "cmpw" } } */

/* Origin:Pete Steinmetz <steinmtz@us.ibm.com> */

/* PR 16458: Extraneous compare.  */

int foo (unsigned a, unsigned b)
{
  if (a == b) return 1;
  if (a > b)  return 2;
  if (a < b)  return 3;
  return 0;
}
