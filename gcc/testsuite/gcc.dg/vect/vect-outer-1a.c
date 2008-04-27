/* { dg-do compile } */

#define N 64
signed short image[N][N] __attribute__ ((__aligned__(16)));
signed short block[N][N] __attribute__ ((__aligned__(16)));

/* Can't do outer-loop vectorization because of non-consecutive access.
   Currently fails to vectorize because the reduction pattern is not
   recognized.  */

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
/* FORNOW */
/* { dg-final { scan-tree-dump-times "strided access in outer loop" 1 "vect" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "unexpected pattern" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
