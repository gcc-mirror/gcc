/* { dg-do compile } */

#define N 256
signed short image[N][N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
signed short block[N][N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

/* Can't do outer-loop vectorization because of non-consecutive access.  */

int
foo (){
  int i,j;
  int diff = 0;

  for (i = 0; i < N; i++) {
    for (j = 0; j < N; j+=8) {
      diff += (image[i][j] - block[i][j]);
    }
  }
  return diff;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "grouped access in outer loop" 1 "vect" { xfail vect_multiple_sizes } } } */
/* { dg-final { scan-tree-dump-times "grouped access in outer loop" 2 "vect" { target vect_multiple_sizes } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
