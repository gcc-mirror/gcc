/* { dg-do run { target { powerpc*-*-* } } } */

/* Test if __builtin_ppc_get_timebase () is compatible with the current
   processor and if it's changing between reads.  A read failure might indicate
   a Power ISA or binutils change.  */

#include <inttypes.h>

int
main (void)
{
  uint64_t t = __builtin_ppc_get_timebase ();
  int j;

  for (j = 0; j < 1000000; j++)
    if (t != __builtin_ppc_get_timebase ())
      return 0;

  return 1;
}
