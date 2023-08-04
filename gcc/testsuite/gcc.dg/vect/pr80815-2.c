/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"
int arr[2048];
int res[100] = { 13198, 13224, 12735, 12760, 12270, 12294,
		 11803, 11826, 11334, 11356, 10863, 10884,
		 10390, 10410, 9915, 9934, 9438, 9456,
		 8959, 8976, 8478, 8494, 7995, 8010,
		 7510, 7524, 7023, 7036, 6534, 6546,
		 6043, 6054, 5550, 5560, 5055, 5064,
		 4558, 4566, 4059, 4066, 3558, 3564,
		 3055, 3060, 2550, 2554, 2043, 0};

__attribute__ ((noinline)) int
foo (int *a, int *b)
{
  int i;
  int *a1 = a;
  int *a0 = a1 - 512;
  for (i = 0; i < 50; i++)
    {
      *b = *a0 + *a1;
      b--;
      a0--;
      a1--;
    }
  return 0;
}

int main (void)
{
  int *a = &arr[1024];
  int *b = &arr[1022];

  int i;
  for (i = 0; i < 2048; i++)
    arr[i] = i;

  foo (a, b);

#pragma GCC novector
  for (i = 973; i < 1020; i++)
    if (arr[i] != res[i - 973])
      abort ();

  return 0;
}
