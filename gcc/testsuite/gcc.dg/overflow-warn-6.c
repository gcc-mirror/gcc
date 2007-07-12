/* Test non-constant operands in overflowed expressions.  */
/* { dg-do compile } */
/* { dg-options "-std=c99 -Woverflow" } */

#include <limits.h>

int 
h1 (int x)
{
  return x * (0 * (INT_MAX + 1)); /* { dg-warning "integer overflow in expression" } */
}

int 
h2 (int x)
{
  return ((INT_MAX + 1) * 0) * x; /* { dg-warning "integer overflow in expression" } */
}

