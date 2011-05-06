#include <stdarg.h>
#include "tree-vect.h"

#define N 128

typedef struct {
   unsigned short a;
   unsigned short b;
   unsigned short c;
} s;

#define A(I) (I)
#define B(I) ((I) * 2)
#define C(I) ((unsigned short) ~((I) ^ 0x18))

void __attribute__ ((noinline))
check1 (s *res)
{
  int i;

  for (i = 0; i < N; i++)
    if (res[i].a != C (i)
	|| res[i].b != A (i)
	|| res[i].c != B (i))
      abort ();
}

void __attribute__ ((noinline))
check2 (unsigned short *res)
{
  int i;

  for (i = 0; i < N; i++)
    if (res[i] != (unsigned short) (A (i) + B (i) + C (i)))
      abort ();
}

void __attribute__ ((noinline))
check3 (s *res)
{
  int i;

  for (i = 0; i < N; i++)
    if (res[i].a != i
	|| res[i].b != i
	|| res[i].c != i)
      abort ();
}

void __attribute__ ((noinline))
check4 (unsigned short *res)
{
  int i;

  for (i = 0; i < N; i++)
    if (res[i] != (unsigned short) (A (i) + B (i)))
      abort ();
}

void __attribute__ ((noinline))
main1 (s *arr)
{
  int i;
  s *ptr = arr;
  s res1[N];
  unsigned short res2[N];

  for (i = 0; i < N; i++)
    {
      res1[i].a = arr[i].c;
      res1[i].b = arr[i].a;
      res1[i].c = arr[i].b;
    }
  check1 (res1);

  for (i = 0; i < N; i++)
    res2[i] = arr[i].a + arr[i].b + arr[i].c;
  check2 (res2);

  for (i = 0; i < N; i++)
    {
      res1[i].a = i;
      res1[i].b = i;
      res1[i].c = i;
    }
  check3 (res1);

  for (i = 0; i < N; i++)
    res2[i] = arr[i].a + arr[i].b;
  check4 (res2);
}

int main (void)
{
  int i;
  s arr[N];

  check_vect ();

  for (i = 0; i < N; i++)
    {
      arr[i].a = A (i);
      arr[i].b = B (i);
      arr[i].c = C (i);
    }
  main1 (arr);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 4 loops" 1 "vect"  { target vect_strided3 } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
