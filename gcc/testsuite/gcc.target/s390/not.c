/* { dg-do compile } */
/* { dg-options "-O3 -march=z15 -mzarch" } */

unsigned long
foo (unsigned long a)
{
  return ~a;
}

/* { dg-final { scan-assembler-times "\tnngrk\t" 1 { target { lp64 } } } } */
/* { dg-final { scan-assembler-times "\tnnrk\t" 1 { target { ! lp64 } } } } */
