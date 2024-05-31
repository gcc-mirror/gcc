/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

#define N 320
#define M 1024
unsigned short in[N+M];
unsigned short coeff[M];
unsigned int out[N];

/* Outer-loop vectorization.  */

void
foo (){
  int i,j;
  unsigned short diff;

  for (i = 0; i < N; i++) {
    diff = 0;
    for (j = 0; j < M; j+=8) {
      diff += in[j+i]*coeff[j];
    }
    out[i]=diff;
  }
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" { target { vect_short_mult && { ! vect_no_align } } } } } */
/* { dg-final { scan-tree-dump-times "zero step in outer loop.(?:(?!Analysys failed).)*Analysis succeeded" 1 "vect" { target { vect_short_mult && { ! vect_no_align } } } } } */
