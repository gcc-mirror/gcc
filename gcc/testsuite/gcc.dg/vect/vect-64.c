/* { dg-do run { target powerpc*-*-* } } */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-stats -maltivec" { target powerpc*-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-stats -msse2" { target i?86-*-* x86_64-*-* } } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

int main1 ()
{
  int i, j;
  int ib[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  int ia[N][4][N+1];
  int ic[N][N][3][13];
  int id[N][N][N];

  /* Multidimensional array. Not aligned: not vectorizable. */
  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
        {
           ia[i][1][j] = ib[i];
        }
    }

  /* Multidimensional array. Aligned: vectorizable. */
  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
        {
           ic[i][1][1][j] = ib[i];
        }
    }

  /* Multidimensional array. Not aligned: not vectorizable. */
  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
        {
           id[i][1][j+1] = ib[i];
        }
    }

  /* check results: */  
  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
        {
           if (ia[i][1][j] != ib[i])
              abort();
        }
    }

  /* check results: */  
  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
        {
           if (ic[i][1][1][j] != ib[i])
              abort();
        }
    }

  /* check results: */  
  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
        {
           if (id[i][1][j+1] != ib[i])
              abort();
        }
    }

  return 0;
}

int main (void)
{ 
  check_vect ();

  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 3 loops" 1 "vect" } } */
