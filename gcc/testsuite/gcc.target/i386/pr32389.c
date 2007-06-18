/* Testcase by Mike Frysinger <vapier@gentoo.org>  */

/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-options "-msse" } */

double f1();
int f2() {
  __builtin_ia32_stmxcsr();
  return f1();
}
