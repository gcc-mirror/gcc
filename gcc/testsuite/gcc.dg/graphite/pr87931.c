/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-copy-prop -fgraphite-identity" } */

#define N 40
#define M 128
float in[N+M];
float coeff[M];
float fir_out[N];

void fir ()
{
  int i,j,k;
  float diff;

  for (i = 0; i < N; i++) {
    diff = 0;
    for (j = 0; j < M; j++) {
      diff += in[j+i]*coeff[j];
    }
    fir_out[i] = diff;
  }
}
