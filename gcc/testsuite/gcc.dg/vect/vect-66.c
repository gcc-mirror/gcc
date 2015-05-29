/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

__attribute__ ((noinline))
void main1 ()
{
  int i, j;
  int ia[8][5][N+2];

  /* Multidimensional array. Aligned. */
  for (i = 0; i < 16; i++)
    {
      for (j = 0; j < N; j++)
        {
           ia[2][6][j] = 5;
        }
    }

  /* check results: */  
  for (i = 0; i < 16; i++)
    {
      for (j = 0; j < N; j++)
        {
           if (ia[2][6][j] != 5)
                abort();
        }
    }
}

__attribute__ ((noinline))
void main2 ()
{
  int i, j;
  int ia[8][5][N+2];

  /* Multidimensional array. Aligned. */
  for (i = 0; i < 16; i++)
    {
      for (j = 0; j < N; j++)
           ia[3][6][j+2] = 5;
    }

  /* check results: */  
  for (i = 0; i < 16; i++)
    {
      for (j = 2; j < N+2; j++)
        {
           if (ia[3][6][j] != 5)
                abort();
        }
    }
}

__attribute__ ((noinline))
void main3 ()
{
  int i, j;
  int ic[16][16][5][N+2];

  /* Multidimensional array. Not aligned. */
  for (i = 0; i < 16; i++)
    {
      for (j = 0; j < N; j++)
        {
           ic[2][1][6][j+1] = 5;
        }
    }

  /* check results: */  
  for (i = 0; i < 16; i++)
    {
      for (j = 0; j < N; j++)
        {
           if (ic[2][1][6][j+1] != 5)
                abort();
        }
    }
}

int main (void)
{ 
  check_vect ();

  main1 ();
  main2 ();
  main3 ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 3 "vect" } } */
