/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 12

struct s{
  int m;
  int n[N][N][N];
};

struct s2{
  int m;
  int n[N-1][N-1][N-1];
};

struct test1{
  struct s a; /* array a.n is unaligned */
  int b;
  int c;
  struct s e; /* array e.n is aligned */
};

struct test2{
  struct s2 a; /* array a.n is unaligned */
  int b;
  int c;
  struct s2 e; /* array e.n is aligned */
};


struct test1 tmp1[4];
struct test2 tmp2[4];

__attribute__ ((noinline))
int main1 ()
{  
  int i,j;

  /* 1. unaligned (known misalignment) */
  for (i = 0; i < N; i++)
    {
      tmp1[2].a.n[1][2][i] = 5;
    }

  /* check results:  */
  for (i = 0; i <N; i++)
    {
      if (tmp1[2].a.n[1][2][i] != 5)
        abort ();
    }

  /* 2. aligned */
  for (i = 3; i < N-1; i++)
    {
      tmp1[2].a.n[1][2][i] = 6;
    }

  /* check results:  */
  for (i = 3; i < N-1; i++)
    {
      if (tmp1[2].a.n[1][2][i] != 6)
        abort ();
    }

  /* 3. aligned */
  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
	{
          tmp1[2].e.n[1][i][j] = 8;
	}
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
	{
          if (tmp1[2].e.n[1][i][j] != 8)
	    abort ();
	}
    }

  /* 4. unaligned (unknown misalignment) */
  for (i = 0; i < N-4; i++)
    {
      for (j = 0; j < N-4; j++)
	{
          tmp2[2].e.n[1][i][j] = 8;
	}
    }

  /* check results:  */
  for (i = 0; i < N-4; i++)
    {
      for (j = 0; j < N-4; j++)
	{
          if (tmp2[2].e.n[1][i][j] != 8)
	    abort ();
	}
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 ();
} 

/* { dg-final { scan-tree-dump-times "vectorized 4 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 2 "vect" { xfail {! vector_alignment_reachable} } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 1 "vect" { target {! vector_alignment_reachable} } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" { target {! vector_alignment_reachable} } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
