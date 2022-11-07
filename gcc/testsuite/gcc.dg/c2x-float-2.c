/* Test INFINITY macro.  Generic test.  */
/* { dg-do run } */
/* { dg-options "-std=c2x -w" } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target inff } */

#include <float.h>

#ifndef INFINITY
#error "INFINITY undefined"
#endif

extern void abort (void);
extern void exit (int);

int
main (void)
{
  (void) _Generic (INFINITY, float : 0);
  if (!(INFINITY >= FLT_MAX))
    abort ();
  exit (0);
}
