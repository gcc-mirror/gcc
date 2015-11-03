/* Check that when taking the complement of the T bit using the negc
   instruction pattern, the constant -1 is loaded only once on non-SH2A and
   that the movrt insn is generated on SH2A.
/* { dg-do compile }  */
/* { dg-options "-O1 -mbranch-cost=2" } */

/* { dg-final { scan-assembler-times "mov\t#-1" 1 { target { ! sh2a } } } } */
/* { dg-final { scan-assembler-times "movrt" 4 { target { sh2a } } } } */

void
testfunc_00 (int* a, int* b, int c, int d)
{
  b[0] = a[0] != c;
  b[1] = a[1] != d;
  b[2] = a[2] != c;
  b[3] = a[3] != d;
}

