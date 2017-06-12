/* PR c/63626 */
/* { dg-do compile } */
/* { dg-options "" } */

/* Test that we don't output the warning twice.  */

inline int foo (void); /* { dg-bogus "inline function.*inline function" } */
/* { dg-warning "inline function .foo. declared but never defined" "" { target *-*-* } .-1 } */
