/* Test declaration specifiers.  Test cases that used to be handled in
   a loop in grokdeclarator.  Pedantic cases.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu89 -pedantic" } */

/* These should all be diagnosed, but only once, not for every
   identifier declared.  */

const const int x0, /* { dg-warning "duplicate" } */
x1;

long long x2, /* { dg-warning "long long" } */
x3;
