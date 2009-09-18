/* Simple test of vararg passing for problematic types with and without
   double values passed between them.  */

#define DTYPE _Decimal64
#define ONE 1.0dd
#define THREE 3.0dd
#define SEVEN 7.0dd
#define ELEVEN 11.0dd
#define INTS 2

#include "func-vararg-alternate.h"

int
main ()
{
  doit ();

  FINISH
}
