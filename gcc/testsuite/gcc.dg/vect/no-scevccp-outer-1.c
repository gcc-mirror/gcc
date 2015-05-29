/* { dg-do compile } */

#define N 40
signed short image[N][N];
signed short block[N][N];

/* memory references in the inner-loop */

__attribute__ ((noinline)) unsigned int
foo (){
  int i,j;
  unsigned int diff = 0;

  for (i = 0; i < N; i++) {
    for (j = 0; j < N; j++) {
      diff += (image[i][j] - block[i][j]);
    }
  }
  return diff;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" { xfail *-*-* } } } */
