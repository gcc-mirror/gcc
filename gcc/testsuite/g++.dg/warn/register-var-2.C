/* PR/18160 */

/* { dg-do compile } */

/* This should yield an error even without -pedantic.  */
/* { dg-options "-Wall -W" } */

void g(int *);

void f(void) 
{ 
  register int x;	/* { dg-warning "ISO C\\+\\+17 does not allow 'register' storage class specifier" "" { target c++17 } } */
  g(&x); /* { dg-warning "address requested for 'x', which is declared 'register'" } */
} 
