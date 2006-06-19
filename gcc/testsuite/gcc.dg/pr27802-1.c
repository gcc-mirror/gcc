/* Noreturn functions returning FP types used to confuse reg-stack on x86.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

double bar1() __attribute__((noreturn));
void foo1() { bar1(); }

double bar2() __attribute__((noreturn));
double foo2() { return bar2(); }

void bar3() __attribute__((noreturn));
double foo3() { bar3(); }

double bar4() __attribute__((noreturn));
double foo4() { bar4(); }

