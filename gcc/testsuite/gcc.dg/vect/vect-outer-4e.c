/* { dg-do compile } */

#define N 40
#define M 128
unsigned int in[N+M];
unsigned short out[N];

/* Outer-loop vectorization. */

void
foo (){
  int i,j;
  unsigned int diff;

  for (i = 0; i < N; i++) {
    diff = 0;
    for (j = 0; j < M; j+=8) {
      diff += in[j+i];
    }
    out[i]=(unsigned short)diff;
  }

  return;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" { xfail *-*-* } } } */
