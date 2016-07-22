/* PR target/70021 */

#include "tree-vect.h"

#define N 160
int a[N];
unsigned long long int b[N], c[N], d[N], e[N];

__attribute__((noinline, noclone)) void
foo (void)
{
  int i;
  for (i = 0; i < N; i += 4)
    {
      unsigned long long int f = (_Bool) b[i];
      unsigned long long int g = c[i] != d[i];
      e[i] = g ^ (a[i] & (g << f));
    }
}

int
main ()
{
  int i;
  check_vect ();
  for (i = 0; i < N; ++i)
    {
      a[i] = 1618000128;
      b[i] = 10919594786573202791ULL;
      c[i] = 2593730175074624973ULL;
      d[i] = 7447894520878803661ULL;
      e[i] = 14234165565810642243ULL;
    }
  foo ();
  for (i = 0; i < N; ++i)
    if (e[i] != ((i & 3) ? 14234165565810642243ULL : 1ULL))
      __builtin_abort ();
  return 0;
}
