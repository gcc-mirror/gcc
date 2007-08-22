/* Testcase by Mike Frysinger <vapier@gentoo.org>  */

/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-msse" } */

double f1();
int f2() {
  __builtin_ia32_stmxcsr();
  return f1();
}
