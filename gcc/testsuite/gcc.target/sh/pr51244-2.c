/* Check that when taking the complement of the T bit using the negc
   instruction pattern, the constant -1 is loaded only once.
   On SH2A this test is skipped because the movrt instruction is used
   to get the complement of the T bit.  */
/* { dg-do compile }  */
/* { dg-options "-O1 -mbranch-cost=2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" "-m2a*" } { "" } } */
/* { dg-final { scan-assembler-times "mov\t#-1" 1 } } */

void
testfunc_00 (int* a, int* b, int c, int d)
{
  b[0] = a[0] != c;
  b[1] = a[1] != d;
  b[2] = a[2] != c;
  b[3] = a[3] != d;
}

