/* { dg-do compile } */
/* { dg-options "-Wmissing-prototypes" } */

inline int foo (void) { return 42; } /* { dg-bogus "no previous prototype" } */
extern int foo(void);
