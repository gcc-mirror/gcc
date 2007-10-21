/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128 

typedef struct {
   unsigned short a;
   unsigned short b;
   unsigned short c;
   unsigned short d;
} s;

unsigned short a[N];
unsigned short b[N];
unsigned short c[N];

__attribute__ ((noinline)) int
main1 (s *arr)
{
  int i;
  s *ptr = arr;
  s res[N];
  unsigned short x, y, z, w;

  for (i = 0; i < N; i++)
    {
      res[i].c = a[i];
      res[i].a = b[i];
      res[i].d = c[i];
      res[i].b = a[i] + b [i];
      ptr++;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (res[i].c != a[i]
          || res[i].a != b[i]
          || res[i].d != c[i]
          || res[i].b != a[i] + b[i])
        abort ();
    }

  return 0;
}

int main (void)
{
  int i;
  s arr[N];

  check_vect ();

  for (i = 0; i < N; i++)
    {
      a[i] = i;
      b[i] = i * 2;
      c[i] = 17;
    }

  main1 (arr);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect"  { target { vect_interleave && vect_pack_trunc  } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target { { ! { vect_interleave } } && { vect_pack_trunc } } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */


