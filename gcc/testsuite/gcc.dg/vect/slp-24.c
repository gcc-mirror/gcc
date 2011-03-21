/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
#define DIFF 242

typedef struct {
   unsigned char a;
   unsigned char b;
   unsigned char c;
   unsigned char d;
} s;

unsigned char ub[N*2] = {1,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,1,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
unsigned char uc[N] = {1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

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
    udiff += (unsigned char)(ub[i] - uc[i]);

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
      abort();
  }

  /* check results:  */
  if (udiff != DIFF)
    abort ();
}

int main (void)
{
  int i; 
  s arr[N];

  for (i = 0; i < N; i++)
    {
      arr[i].a = i + 9;
      arr[i].b = i * 2 + 10;
      arr[i].c = 17;
      arr[i].d = i+34;
      if (arr[i].a == 178)
         abort();
    }
  check_vect ();
  
  main1 (100, 100, 1, arr);
  main1 (0, 15, 0, arr);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail { vect_no_align && ilp32 } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect" { xfail { vect_no_align && ilp32 } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
