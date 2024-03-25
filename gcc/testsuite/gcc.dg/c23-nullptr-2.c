/* Test nullptr_t from <stddef.h>.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#include <stddef.h>

void f(nullptr_t);
_Static_assert (sizeof (nullptr_t) == sizeof (char *), "sizeof (nullptr_t)");
_Static_assert (_Alignof (nullptr_t) == _Alignof (char *), "_Alignof (nullptr_t)");
