/* { dg-do compile } */

#define N 40
#define M 128
signed short in[N+M];
signed short coeff[M];
signed short out[N];

/* Outer-loop vectorization. */

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

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" { target { vect_widen_mult_hi_to_si && vect_pack_trunc } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
