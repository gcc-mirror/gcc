/* Functional tests for the -mpreserve-args cmdline option.  */

/* { dg-do compile } */
/* { dg-options "-O3 -march=z900 -mpreserve-args" } */


int
foo (int a, int b, int c, double d, double e)
{
  return a + c + (int)d + (int)e;
}

/* { dg-final { scan-assembler "stmg\t%r2,%r4,\[0-9\]*\\(%r15\\)" { target lp64 } } } */
/* { dg-final { scan-assembler "stm\t%r2,%r4,\[0-9\]*\\(%r15\\)" { target { ! lp64 } } } } */

/* { dg-final { scan-assembler "std\t%f0,\[0-9\]*\\(%r15\\)" } } */
/* { dg-final { scan-assembler "std\t%f2,\[0-9\]*\\(%r15\\)" } } */
