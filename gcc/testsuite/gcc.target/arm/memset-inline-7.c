/* { dg-do run } */
/* { dg-options "-O2" } */

#include <string.h>
#include <stdlib.h>

#define LEN (100)
short a[LEN];
int b[LEN];

void
init (signed char *arr, int len)
{
  int i;
  for (i = 0; i < len; i++)
    arr[i] = 0;
}

void
check (signed char *arr, int idx, int len, int v)
{
  int i;
  for (i = 0; i < idx; i++)
    if (arr[i] != v)
      abort ();

  for (i = idx; i < len; i++)
    if (arr[i] != 0)
      abort ();
}

#define TEST(a,l,v)			\
	init ((signed char*)(a), sizeof (a));		\
	memset ((a), (v), (l));				\
	check ((signed char *)(a), (l), sizeof (a), (v));
int
main(void)
{
  TEST (a, 1, -1);
  TEST (a, 2, -1);
  TEST (a, 3, -1);
  TEST (a, 4, -1);
  TEST (a, 5, -1);
  TEST (a, 6, -1);
  TEST (a, 7, -1);
  TEST (a, 8, -1);
  TEST (a, 9, 1);
  TEST (a, 10, -1);
  TEST (a, 11, 1);
  TEST (a, 12, -1);
  TEST (a, 13, 1);
  TEST (a, 14, -1);
  TEST (a, 15, 1);
  TEST (a, 16, -1);
  TEST (a, 17, 1);
  TEST (a, 18, -1);
  TEST (a, 19, 1);
  TEST (a, 20, -1);
  TEST (a, 21, 1);
  TEST (a, 22, -1);
  TEST (a, 23, 1);
  TEST (a, 24, -1);
  TEST (a, 25, 1);
  TEST (a, 26, -1);
  TEST (a, 27, 1);
  TEST (a, 28, -1);
  TEST (a, 29, 1);
  TEST (a, 30, -1);
  TEST (a, 31, 1);
  TEST (a, 32, -1);
  TEST (a, 33, 1);
  TEST (a, 34, -1);
  TEST (a, 35, 1);
  TEST (a, 36, -1);
  TEST (a, 37, 1);
  TEST (a, 38, -1);
  TEST (a, 39, 1);
  TEST (a, 40, -1);
  TEST (a, 41, 1);
  TEST (a, 42, -1);
  TEST (a, 43, 1);
  TEST (a, 44, -1);
  TEST (a, 45, 1);
  TEST (a, 46, -1);
  TEST (a, 47, 1);
  TEST (a, 48, -1);
  TEST (a, 49, 1);
  TEST (a, 50, -1);
  TEST (a, 51, 1);
  TEST (a, 52, -1);
  TEST (a, 53, 1);
  TEST (a, 54, -1);
  TEST (a, 55, 1);
  TEST (a, 56, -1);
  TEST (a, 57, 1);
  TEST (a, 58, -1);
  TEST (a, 59, 1);
  TEST (a, 60, -1);
  TEST (a, 61, 1);
  TEST (a, 62, -1);
  TEST (a, 63, 1);
  TEST (a, 64, -1);

  TEST (b, 1, -1);
  TEST (b, 2, -1);
  TEST (b, 3, -1);
  TEST (b, 4, -1);
  TEST (b, 5, -1);
  TEST (b, 6, -1);
  TEST (b, 7, -1);
  TEST (b, 8, -1);
  TEST (b, 9, 1);
  TEST (b, 10, -1);
  TEST (b, 11, 1);
  TEST (b, 12, -1);
  TEST (b, 13, 1);
  TEST (b, 14, -1);
  TEST (b, 15, 1);
  TEST (b, 16, -1);
  TEST (b, 17, 1);
  TEST (b, 18, -1);
  TEST (b, 19, 1);
  TEST (b, 20, -1);
  TEST (b, 21, 1);
  TEST (b, 22, -1);
  TEST (b, 23, 1);
  TEST (b, 24, -1);
  TEST (b, 25, 1);
  TEST (b, 26, -1);
  TEST (b, 27, 1);
  TEST (b, 28, -1);
  TEST (b, 29, 1);
  TEST (b, 30, -1);
  TEST (b, 31, 1);
  TEST (b, 32, -1);
  TEST (b, 33, 1);
  TEST (b, 34, -1);
  TEST (b, 35, 1);
  TEST (b, 36, -1);
  TEST (b, 37, 1);
  TEST (b, 38, -1);
  TEST (b, 39, 1);
  TEST (b, 40, -1);
  TEST (b, 41, 1);
  TEST (b, 42, -1);
  TEST (b, 43, 1);
  TEST (b, 44, -1);
  TEST (b, 45, 1);
  TEST (b, 46, -1);
  TEST (b, 47, 1);
  TEST (b, 48, -1);
  TEST (b, 49, 1);
  TEST (b, 50, -1);
  TEST (b, 51, 1);
  TEST (b, 52, -1);
  TEST (b, 53, 1);
  TEST (b, 54, -1);
  TEST (b, 55, 1);
  TEST (b, 56, -1);
  TEST (b, 57, 1);
  TEST (b, 58, -1);
  TEST (b, 59, 1);
  TEST (b, 60, -1);
  TEST (b, 61, 1);
  TEST (b, 62, -1);
  TEST (b, 63, 1);
  TEST (b, 64, -1);

  return 0;
}

