/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"
int arr[2048];
int res[100] = { 2148, 2146, 2144, 2142, 2140, 2138, 2136, 2134, 2132, 2130,
		 2128, 2126, 2124, 2122, 2120, 2118, 2116, 2114, 2112, 2110,
		 2108, 2106, 2104, 2102, 2100, 2098, 2096, 2094, 2092, 2090,
		 2088, 2086, 2084, 2082, 2080, 2078, 2076, 2074, 2072, 2070,
		 2068, 2066, 2064, 2062, 2060, 2058, 2056, 2054, 3078, 2050};

__attribute__ ((noinline)) int
foo (int *a, int *b, int len)
{
  int i;
  int *a1 = a;
  int *a0 = a1 - 4;
  for (i = 0; i < len; i++)
    {
      *b = *a0 + *a1;
      b--;
      a0++;
      a1++;
    }
  return 0;
}

int main (void)
{
  int *a = &arr[1027];
  int *b = &arr[1024];

  int i;
  for (i = 0; i < 2048; i++)
    arr[i] = i;

  foo (a, b, 50);

#pragma GCC novector
  for (i = 975; i < 1025; i++)
    if (arr[i] != res[i - 975])
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "improved number of alias checks from \[0-9\]* to 1" "vect" { target vect_perm } } } */
