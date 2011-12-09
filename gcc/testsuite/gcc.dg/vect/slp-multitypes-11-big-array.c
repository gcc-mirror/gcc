/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 144

struct s
{
  int a;
  int b;
  int c;
};

char in[N*3];
volatile int y = 0;

__attribute__ ((noinline)) int
main1 ()
{
  int i;
  struct s out[N];

  for (i = 0; i < N; i++)
    {
      in[i] = i&127;
      if (y) /* Avoid vectorization.  */
	abort ();
    }

  for (i = 0; i < N; i++)
    {
      out[i].a = (int) in[i*3] + 1;
      out[i].b = (int) in[i*3 + 1] + 2;
      out[i].c = (int) in[i*3 + 2] + 3;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (out[i].a !=  (int) in[i*3] + 1
         || out[i].b != (int) in[i*3 + 1] + 2
         || out[i].c != (int) in[i*3 + 2] + 3)
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

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target vect_unpack } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect"  { target vect_unpack } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

