// Test case from PR 7470, which was fixed in GCC 3.2 and breaks binary
// compatibility with earlier releases.
// Split into pieces for binary compatibility testing October 2002

#include "vbase11.h"

extern void vbase11_x (void);

int
main ()
{
  vbase11_x ();
}
