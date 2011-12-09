/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

typedef struct {
   unsigned char a;
   unsigned char b;
   unsigned char c;
   unsigned char d;
} s;

unsigned char ub[N*2];
unsigned char uc[N];

volatile int y = 0;
unsigned char check_diff = 2;

void
main1 (unsigned char x, unsigned char max_result, unsigned char min_result, s *arr)
{
  int i;
  unsigned char udiff = 2;
  unsigned char umax = x;
  unsigned char umin = x;
  unsigned char ua1[N*2];
  s *pIn = arr;
  s out[N];

  for (i = 0; i < N; i++) {
    udiff += (unsigned char) (ub[i] - uc[i]);

    ua1[2*i+1] = ub[2*i+1];
    ua1[2*i] = ub[2*i];

    out[i].d = pIn->d - 1;
    out[i].b = pIn->b - 4;
    out[i].c = pIn->c - 8;
    out[i].a = pIn->a - 3;

    pIn++;
  }

  for (i = 0; i < N; i++) {
    if (ua1[2*i] != ub[2*i]
        || ua1[2*i+1] != ub[2*i+1]
        || out[i].a != arr[i].a - 3
        || out[i].b != arr[i].b - 4
        || out[i].c != arr[i].c - 8
        || out[i].d != arr[i].d - 1)
      abort ();
  }

  /* check results:  */
  if (udiff != check_diff)
    abort ();
}

int main (void)
{
  int i;
  s arr[N];

  check_diff = 2;
  ub[0] = uc[0] = 1;
  for (i = 1; i < N; i++) {
    ub[i] = (i%5 == 0)?i*3:i;
    uc[i] = i;
    check_diff += (unsigned char) (ub[i] - uc[i]);
    if (y) /* Avoid vectorization.  */
      abort ();
  }
  for (; i < 2*N; i++) {
    ub[i] = 0;
    if (y) /* Avoid vectorization.  */
      abort ();
  }

  for (i = 0; i < N; i++)
    {
      arr[i].a = i + 9;
      arr[i].b = i * 2 + 10;
      arr[i].c = 17;
      arr[i].d = i+34;
      if (arr[i].a == 178)
         abort ();
    }
  check_vect ();

  main1 (100, 100, 1, arr);
  main1 (0, 15, 0, arr);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail { vect_no_align && ilp32 } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect" { xfail { vect_no_align && ilp32 } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
