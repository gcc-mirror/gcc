/* Test unreachable in <stddef.h> for C2x.  */
/* { dg-do run } */
/* { dg-options "-std=c2x -pedantic-errors -O2" } */

#include <stddef.h>

#ifndef unreachable
#error "unreachable not defined"
#endif

extern void *p;
extern __typeof__ (unreachable ()) *p;

volatile int x = 1;

extern void not_defined (void);

extern void exit (int);

int
main ()
{
  if (x == 2)
    {
      unreachable ();
      not_defined ();
    }
  exit (0);
}
