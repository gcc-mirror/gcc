/* Test C11 alignment support.  Test code valid after the resolution
   of DR#444: alignment specifiers for struct and union members and
   compound literals.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <stddef.h>

struct s
{
  _Alignas (_Alignof (max_align_t)) char c;
};

union u
{
  _Alignas (_Alignof (max_align_t)) char c;
};

char *p = &(_Alignas (_Alignof (max_align_t)) char) { 1 };
size_t size = sizeof (_Alignas (_Alignof (max_align_t)) char) { 1 };
