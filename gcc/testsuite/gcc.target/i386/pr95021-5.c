/* { dg-do run { target int128 } } */
/* { dg-require-effective-target sse2_runtime } */
/* { dg-options "-O2 -msse2 -mstv -W" } */

#include <stdlib.h>
#include "pr95021-4.c"

jmp_buf buf;

__int128 *target_p;
__int128 *c;

int count;

__attribute__ ((noclone, noinline))
void
foo (__int128 i1, __int128 i2, __int128 i3, __int128 x)
{
  if (i1 != 0xbadbeef1)
    abort ();
  if (i2 != 0x2badbeef)
    abort ();
  if (i3 != 0xbad3beef)
    abort ();
  if (x != *c)
    abort ();
  if (!count)
    {
      count++;
      longjmp (buf, 1);
    }
}

int
main ()
{
  __int128 val = 30;
  __int128 local = 0;
  target_p = &val;
  c = &local;
  bar ();
  if (val != local)
    abort ();
  return 0;
}
