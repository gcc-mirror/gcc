/* Test C11 alignment support.  Test invalid use of alignment
   specifiers in type names in cases not permitted by the resolution
   of DR#444.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <stddef.h>

void
f (void)
{
  _Generic (1, int: 1, _Alignas (8) long: 2); /* { dg-error "expected" } */
  sizeof (_Alignas (8) long); /* { dg-error "specified for type name" } */
  _Alignof (_Alignas (8) long); /* { dg-error "specified for type name" } */
  (_Alignas (8) long) 0; /* { dg-error "specified for type name" } */
  _Atomic (_Alignas (8) long) x; /* { dg-error "expected" } */
  _Alignas (_Alignas (8) long) long y; /* { dg-error "expected" } */
}
