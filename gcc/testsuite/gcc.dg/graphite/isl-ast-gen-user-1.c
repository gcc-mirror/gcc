/* { dg-do compile } */
/* { dg-options "-O2 -fgraphite-identity -fgraphite-code-generator=isl" } */

#include <stdio.h>
#include <stdlib.h>

static const int N = 12;

void Crystal_Cholesky (int nSlip, int a[N][N])
{
  int i, j, k, fdot = 0;

  for ( i = 1; i < nSlip; i++)
    {
      for ( j = i+1; j < nSlip; j++)
        {
          for ( k = 0; k < i; k++)
            fdot += a[i][k] * a[k][j];
          a[i][j] = a[i][j] - fdot;
        }
   }
}



