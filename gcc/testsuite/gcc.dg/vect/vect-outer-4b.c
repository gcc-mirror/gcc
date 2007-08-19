/* { dg-do compile } */

#define N 40
#define M 128
signed short in[N+M];
signed short coeff[M];
int out[N];

/* Outer-loop vectorization.
   Currently not vectorized because of multiple-data-types in the inner-loop.  */

void
foo (){
  int i,j;
  int diff;

  for (i = 0; i < N; i++) {
    diff = 0;
    for (j = 0; j < M; j+=8) {
      diff += in[j+i]*coeff[j]; 
    }
    out[i]=diff;
  }
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" { xfail *-*-* } } } */
/* FORNOW. not vectorized until we support 0-stride acceses like coeff[j]. should be:
   { scan-tree-dump-not "multiple types in nested loop." "vect" { xfail *-*-* } } } */

/* { dg-final { scan-tree-dump-times "zero step in outer loop." 1  "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
