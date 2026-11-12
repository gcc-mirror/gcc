/* PR tree-optimization/128269 */
/* { dg-do run } */
/* { dg-require-effective-target wchar } */

#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>

volatile int v = 128;
volatile int sink, neg;

/* The conversion may fail for a wide character the locale cannot encode,
   the failure must remain visible to the caller.  */
static inline __attribute__((always_inline)) void
check (int r)
{
  sink = r;
  neg = r < 0;
  if (neg != (sink < 0))
    abort ();
}

int
main (void)
{
  char b[32];
  int x = v;
  if (x >= 0 && x <= 200)
    check (snprintf (b, sizeof b, "%lc", (wint_t) x));
  /* Ranges with negative values convert to large wide characters.  */
  v = -1;
  x = v;
  if (x >= -5 && x <= 100)
    check (snprintf (b, sizeof b, "%lc", x));
  if (x >= -5 && x <= -1)
    check (snprintf (b, sizeof b, "%lc", x));
  return 0;
}
