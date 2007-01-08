/* Test non-constant operands in overflowed expressions.  */
/* { dg-do compile } */
/* { dg-options "-Woverflow" } */

#include <limits.h>

int 
h1 (int x)
{
  return x * (0 * (INT_MAX + 1)); /* { dg-warning "warning: integer overflow in expression" } */
}

int 
h2 (int x)
{
  return ((INT_MAX + 1) * 0) * x; /* { dg-warning "warning: integer overflow in expression" } */
}

