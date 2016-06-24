/* PR tree-optimization/70354 */

#ifndef main
#include "tree-vect.h"
#endif

long long int b[64], c[64], g[64];
unsigned long long int a[64], d[64], e[64], f[64], h[64];

__attribute__ ((noinline, noclone)) void
foo (void)
{
  int i;
  for (i = 0; i < 64; i++)
    {
      d[i] = h[i] << (((((unsigned long long int) b[i] * e[i])
			<< (-a[i] - 3752448776177690134ULL))
		       - 8214565720323784703ULL) - 1ULL);
      e[i] = (_Bool) (f[i] + (unsigned long long int) g[i]);
      g[i] = c[i];
    }
}

int
main ()
{
  int i;
#ifndef main
  check_vect ();
#endif
  if (__CHAR_BIT__ != 8 || sizeof (long long int) != 8)
    return 0;
  for (i = 0; i < 64; ++i)
    {
      a[i] = 14694295297531861425ULL;
      b[i] = -1725558902283030715LL;
      c[i] = 4402992416302558097LL;
      e[i] = 6297173129107286501ULL;
      f[i] = 13865724171235650855ULL;
      g[i] = 982871027473857427LL;
      h[i] = 8193845517487445944ULL;
    }
  foo ();
  for (i = 0; i < 64; i++)
    if (d[i] != 8193845517487445944ULL || e[i] != 1
	|| g[i] != 4402992416302558097ULL)
      abort ();
  return 0;
}
