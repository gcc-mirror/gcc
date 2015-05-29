/* { dg-require-effective-target vect_int } */
/* { dg-skip-if "AArch64 tiny code model does not support programs larger than 1MiB" {aarch64_tiny} {"*"} {""} } */
/* { dg-add-options bind_pic_locally } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

struct s{
  int m;
  int n[N][N][N];
};

struct test1{
  struct s a; /* array a.n is unaligned */
  int b;
  int c;
  struct s e; /* array e.n is aligned */
};

struct test1 tmp1;

__attribute__ ((noinline))
int main1 ()
{  
  int i,j;

  /* 1. unaligned */
  for (i = 0; i < N; i++)
    {
      tmp1.a.n[1][2][i] = 5;
    }

  /* check results:  */
  for (i = 0; i <N; i++)
    {
      if (tmp1.a.n[1][2][i] != 5)
        abort ();
    }

  /* 2. aligned */
  for (i = 3; i < N-1; i++)
    {
      tmp1.a.n[1][2][i] = 6;
    }

  /* check results:  */
  for (i = 3; i < N-1; i++)
    {
      if (tmp1.a.n[1][2][i] != 6)
        abort ();
    }

  /* 3. aligned */
  for (i = 0; i < N; i++)
    {
      tmp1.e.n[1][2][i] = 7;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (tmp1.e.n[1][2][i] != 7)
        abort ();
    }

  /* 4. unaligned */
  for (i = 3; i < N-3; i++)
    {
      tmp1.e.n[1][2][i] = 8;
    }
 
  /* check results:  */
  for (i = 3; i <N-3; i++)
    {
      if (tmp1.e.n[1][2][i] != 8)
        abort ();
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
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 4 "vect" { target { {! vect_aligned_arrays} && {vect_sizes_32B_16B} } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 2 "vect" { target { {vect_aligned_arrays} && {! vect_sizes_32B_16B} } } } } */
