/* { dg-do compile } */
#define N 40

int
foo (){
  int i,j;
  int diff = 0;

  for (i = 0; i < N; i++) {
    for (j = 0; j < N; j++) {
      diff += j;
    }
  }
  return diff;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
