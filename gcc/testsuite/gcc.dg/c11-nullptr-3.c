/* Test there is no nullptr_t in <stddef.h> for C11.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <stddef.h>

int nullptr_t;
