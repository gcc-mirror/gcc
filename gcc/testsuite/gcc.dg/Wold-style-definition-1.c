/* Test for warning about old-style function definition.  */

/* Origin: Andreas Jaeger <aj@suse.de> */
/* { dg-do compile } */
/* { dg-options "-Wold-style-definition" } */

void
bar (a) int a; { } /* { dg-warning "old-style parameter declaration" } */

void bar1 () {} /* { dg-warning "old-style parameter declaration" } */

extern void bar2 (void);

void bar2 () {} /* { dg-warning "old-style parameter declaration" } */

extern void bar3 (int);

void bar3 (a) {} /* { dg-warning "old-style parameter declaration" } */

void bar4 (a) {} /* { dg-warning "old-style parameter declaration" } */

void bar5 (int a) {}

void bar6 (void) {}
