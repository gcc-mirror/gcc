/* { dg-do compile } */
/* { dg-require-effective-target strub } */

void __attribute__ ((strub("internal")))
f(volatile short s) {
}

void g(void) {
  f(0);
}
