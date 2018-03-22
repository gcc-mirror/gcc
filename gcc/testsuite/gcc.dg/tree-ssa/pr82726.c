/* { dg-do compile } */
/* { dg-options "-O3 --param tree-reassoc-width=4" } */
/* { dg-additional-options "-mavx2" { target { x86_64-*-* i?86-*-* } } } */

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
