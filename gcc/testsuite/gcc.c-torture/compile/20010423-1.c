/* Origin: PR c/2618 from Cesar Eduardo Barros <cesarb@nitnet.com.br>,
   adapted to a testcase by Joseph Myers <jsm28@cam.ac.uk>.

   Boolean conversions were causing infinite recursion between convert
   and fold in certain cases.  */

#include <stdbool.h>

bool x;
unsigned char y;

void
fn (void)
{
  x = y & 0x1 ? 1 : 0;
}
