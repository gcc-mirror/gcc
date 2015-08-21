/* { dg-do compile } */
/* { dg-options "-fprofile-arcs" } */
/* { dg-require-profiling "-fprofile-generate" } */

extern void bar(void) __attribute__((noreturn));
int foo (void) { bar(); }
