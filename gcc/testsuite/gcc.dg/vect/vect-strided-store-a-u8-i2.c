/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64 

typedef struct {
   unsigned char a;
   unsigned char b;
} s;

__attribute__ ((noinline)) int
main1 ()
{
  s arr[N];
  s *ptr = arr;
  s res[N];
  int i;
  unsigned char a[N], b[N];


  for (i = 0; i < N; i++)
    {
      a[i] = i;
      b[i] = i * 2;
      if (i%3 == 0)
        a[i] = 10; 
    }

  for (i = 0; i < N; i++)
    {
      res[i].a = a[i] + 3;
      res[i].b = a[i] + b[i];
      ptr++;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (res[i].a != a[i] + 3
          || res[i].b != a[i] + b[i])
        abort ();
    }

  return 0;
}

int main (void)
{
  check_vect ();

  main1 ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target { vect_interleave } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

