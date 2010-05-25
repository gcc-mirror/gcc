/* { dg-require-alias "" } */
#include "20081222_0.h"

extern void abort (void);

int
main ()
{
  if (x () == 7)
    return 0;
  abort ();
}
