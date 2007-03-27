/* { dg-options "-std=gnu99" } */

/* Simple test of vararg passing for problematic types with and without
   double values passed between them.  */

#define DTYPE _Decimal32
#define ONE 1.0df
#define THREE 3.0df
#define SEVEN 7.0df
#define ELEVEN 11.0df
#define INTS 1

#include "func-vararg-alternate.h"

int
main ()
{
  doit ();
  return 0;
}
