/* { dg-do compile } */
/* { dg-options "-O2" } */
int foo() __attribute__((noreturn));
int bar() { return foo(); }
