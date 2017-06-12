/* PR c/19978 : Test for duplicated warnings (binary operators).  */
/* { dg-do compile } */
/* { dg-options "-Woverflow" } */

#include <limits.h>

int 
g1 (void)
{
  return INT_MAX + 1 - INT_MAX; /* { dg-bogus "integer overflow in expression.*integer overflow in expression" } */
  /* { dg-warning "integer overflow in expression" "" { target *-*-* } .-1 } */
}
