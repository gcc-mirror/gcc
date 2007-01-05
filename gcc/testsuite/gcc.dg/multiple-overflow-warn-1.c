/* PR c/19978 : Test for duplicated warnings (unary operators).  */
/* { dg-do compile } */
/* { dg-options "-std=c99 -Woverflow" } */

#include <limits.h>

int 
g (void)
{
  return - - - - -INT_MIN; /* { dg-bogus "integer overflow in expression.*integer overflow in expression" } */
  /* { dg-warning "integer overflow in expression" "" { target *-*-* } 10 } */
}

