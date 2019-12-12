/* { dg-do compile } */
/* { dg-options "-O3 -mzarch" } */

/* a is used after the comparison.  We cannot use load and test here
   since it would turn SNaNs into QNaNs.  */

double gl;

double
foo (double dummy, double a)
{
  if (a == 0.0)
    gl = 1;
  return a;
}

/* { dg-final { scan-assembler {\tcdbr?\t} } } */
