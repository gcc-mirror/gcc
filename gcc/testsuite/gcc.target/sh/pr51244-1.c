/* Check that inverted conditional branch logic does not generate
   unnecessary explicit T bit extractions, inversions and 
   test instructions.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O1 -mbranch-cost=2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } } */
/* { dg-final { scan-assembler-not "tst|negc|extu" } } */

int
testfunc_00 (int a, int b, int c, int d)
{
  return (a != b || a != d) ? b : c;
}

int
testfunc_01 (int a, char* p, int b, int c)
{
  return (a == b && a == c) ? b : c;
}

int
testfunc_02 (int a, char* p, int b, int c)
{
  return (a == b && a == c) ? b : c;
}

int
testfunc_03 (int a, char* p, int b, int c)
{
  return (a != b && a != c) ? b : c;
}

