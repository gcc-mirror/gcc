/* { dg-do run } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-additional-options "-O3 -fno-inline -std=gnu99" } */
/* { dg-add-options arm_neon } */

typedef struct { int x, y; } X;

void f4(X *p, int n)
{
  for (int i = 0; i < n; i++)
  { p[i].x = i;
    p[i].y = i + 1;
  }
}

__attribute ((aligned (16))) X arr[100];

int main(void)
{
  volatile int fail = 0;
  f4 (arr, 100);
  for (int i = 0; i < 100; i++)
    if (arr[i].y != i+1 || arr[i].x != i)
      fail = 1;
  if (fail)
     __builtin_abort ();

  return 0;
}
