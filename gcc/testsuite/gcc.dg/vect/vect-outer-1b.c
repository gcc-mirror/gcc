/* { dg-do compile } */

#define N 40
signed short image[N][N];
signed short block[N][N];
signed short out[N];

/* Outer-loop cannot get vectorized because of non-consecutive access.  */

void
foo (){
  int i,j;
  int diff;

  for (i = 0; i < N; i++) {
    diff = 0;
    for (j = 0; j < N; j+=4) {
      diff += (image[i][j] - block[i][j]);
    }
    out[i]=diff;
  }
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "grouped access in outer loop" 1 "vect" { target { ! vect_multiple_sizes } } } } */
/* { dg-final { scan-tree-dump-times "grouped access in outer loop" 2 "vect" { target vect_multiple_sizes } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
