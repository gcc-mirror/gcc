#define SIZE 10000

static double P[SIZE][SIZE];

void sor(int N1, int N2){
  int i, j, k;

#pragma scop
  for(i=1; i<N1-1; i++) {
    for(j=1; j<N2-1; j++) {
      P[i][j] = (P[i][j] + P[i][j-1] + P[i][j+1] + P[i-1][j] + P[i+1][j]) / 5;
    }
  }
#pragma endscop
}

/* { dg-final { scan-tree-dump-times "number of SCoPs: 1" 1 "graphite" } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
