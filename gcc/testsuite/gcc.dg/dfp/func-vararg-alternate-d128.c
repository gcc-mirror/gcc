/* { dg-options "-std=gnu99" } */

/* Simple test of vararg passing for problematic types with and without
   double values passed between them.  */

#define DTYPE _Decimal128
#define ONE 1.0dl
#define THREE 3.0dl
#define SEVEN 7.0dl
#define ELEVEN 11.0dl
#define INTS 4

#include "func-vararg-alternate.h"

int
main ()
{
  doit ();
  return 0;
}
