/* { dg-options "-O0 -mlong-double-128" } */

#include <stdio.h>
#include <stdlib.h>

void
check_ll (long double ld, long long ll)
{
  if ((long long)ld != ll)
    {
      printf ("ld: %Lf expect: %lld result: %lld\n",
	      ld, ll, (long long)ld);
      abort ();
    }
}

void
check_ull (long double ld, unsigned long long ull)
{
  if ((unsigned long long)ld != ull)
    {
      printf ("ld: %Lf expect: %llu result: %llu\n",
	      ld, ull, (unsigned long long)ld);
      abort ();
    }
}

int
main ()
{
  const long long ll_max = (long long)((1ULL << 63) - 1);
  const long long ll_min = -ll_max - 1;

  check_ll (206.23253, 206LL);
  check_ull (206.23253, 206ULL);
  check_ll ((long double)ll_max, ll_max);
  check_ull ((long double)ll_max, ll_max);
  check_ll ((long double)ll_min, ll_min);
  check_ll (0.0, 0);
  check_ull (0.0, 0);
  check_ll (-1.0, -1);
  check_ll ((long double)0xffffffffffffffffULL, ll_max);
  check_ull ((long double)0xffffffffffffffffULL, 0xffffffffffffffffULL);

  return 0;
}
