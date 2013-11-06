/* Check that when taking the complement of the T bit on SH2A, 
   the movrt instruction is being generated.  */
/* { dg-do compile }  */
/* { dg-options "-O1 -mbranch-cost=2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "*" } { "-m2a*" } } */
/* { dg-final { scan-assembler-times "movrt" 4 } } */

void
testfunc_00 (int* a, int* b, int c, int d)
{
  b[0] = a[0] != c;
  b[1] = a[1] != d;
  b[2] = a[2] != c;
  b[3] = a[3] != d;
}

