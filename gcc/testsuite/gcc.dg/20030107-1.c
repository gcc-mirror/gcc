/* { dg-do compile } */
/* { dg-options "-fprofile-arcs" } */

extern void bar(void) __attribute__((noreturn));
int foo (void) { bar(); }

/* { dg-final { cleanup-coverage-files } } */
