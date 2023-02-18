/* Functional tests for the -mpreserve-args cmdline option.  */

/* { dg-do compile } */
/* { dg-options "-O3 -march=z900 -mpreserve-args" } */

#include <stdarg.h>
int
foo (int a, int, int c, double d, ...)
{
  va_list argp;
  va_start(argp, d);
  return a + c + va_arg(argp, int) + va_arg(argp, int) + (int)va_arg(argp, double);
}

/* { dg-final { scan-assembler "stmg\t%r2,%r15,\[0-9\]*\\(%r15\\)" { target lp64 } } } */
/* { dg-final { scan-assembler "stm\t%r2,%r15,\[0-9\]*\\(%r15\\)" { target { ! lp64 } } } } */

/* { dg-final { scan-assembler "std\t%f0,\[0-9\]*\\(%r15\\)" } } */
/* { dg-final { scan-assembler "std\t%f2,\[0-9\]*\\(%r15\\)" } } */
