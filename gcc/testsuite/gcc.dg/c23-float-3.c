/* Test INFINITY macro.  Test when infinities supported.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target inff } */

#include <float.h>

#ifndef INFINITY
#error "INFINITY undefined"
#endif

volatile float f = INFINITY;

extern void abort (void);
extern void exit (int);

int
main (void)
{
  (void) _Generic (INFINITY, float : 0);
  if (!(INFINITY > FLT_MAX))
    abort ();
  if (!(f > FLT_MAX))
    abort ();
  exit (0);
}
