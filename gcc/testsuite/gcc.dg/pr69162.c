/* PR tree-optimization/69162 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <stdarg.h>

int
foo (void *a)
{
  va_list *b = a;
  return va_arg (*b, int);
}
