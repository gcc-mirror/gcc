/* { dg-options "-O2" } */

double
load_one (double *in)
{
  return in[400] + in[401] + in[527] + in[528];
}

double
load_two (double *in)
{
  return in[400] + in[401] + in[464] + in[465];
}

/* This is expected to fail due to PR 82214.  */
/* { dg-final { scan-assembler-times "stp\td\[0-9\]+, d\[0-9\]+," 4 { xfail *-*-* } } } */
