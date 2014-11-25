/* { dg-do compile } */
/* { dg-options "-Wmissing-declarations" } */

inline int foo (void) { return 42; } /* { dg-bogus "no previous declaration" } */
extern int foo (void);
