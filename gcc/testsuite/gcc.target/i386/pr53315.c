/* PR target/53315 and PR target/53291 */
/* { dg-do run } */
/* { dg-options "-O2 -mrtm" } */
/* { dg-require-effective-target rtm } */

#include <x86intrin.h>
#include "rtm-check.h"

static void
rtm_test (void)
{
  int flag = -1;
  unsigned status;

  if ((status = _xbegin ()) == _XBEGIN_STARTED)
    {
      flag = _xtest ();
      _xend ();
    }
  else
    return;

  if (flag != 1)
    abort ();
  if (_xtest () != 0)
    abort ();
}
