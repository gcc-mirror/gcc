/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_float } */

#define N 128

void fbar (float *);
void ibar (int *);
void sbar (short *);

/* multiple loops */

void
foo (int n)
{
  float a[N+1];
  float b[N];
  float c[N];
  float d[N];
  int ia[N];
  int ib[N];
  int ic[N];
  int i,j;
  int diff = 0;
  char cb[N];
  char cc[N];
  char image[N][N];
  char block[N][N];

  /* Vectorizable.  */
  diff = 0;
  for (i = 0; i < N; i++) {
    diff += (cb[i] - cc[i]);
  }
  ibar (&diff);


  /* Vectorizable.  */
  diff = 0;
  for (i = 0; i < N; i++) {
    for (j = 0; j < N; j++) {
      diff += (image[i][j] - block[i][j]);
    }
  }
  ibar (&diff);


  /* Vectorizable.  */
  for (i = 0; i < N; i++){
    a[i] = b[i];
  }
  fbar (a);


  /* Vectorizable.  */
  for (i = 0; i < N; i++){
    a[i] = b[i] + c[i] + d[i];
  }
  fbar (a);


  /* Strided access.  Vectorizable on platforms that support load of strided
     accesses (extract of even/odd vector elements).  */
  for (i = 0; i < N/2; i++){
    a[i] = b[2*i+1] * c[2*i+1] - b[2*i] * c[2*i];
    d[i] = b[2*i] * c[2*i+1] + b[2*i+1] * c[2*i];
  }
  fbar (a);


  /* Vectorizable.  */
  for (i = 0; i < N; i++){
    a[i] = b[i] + c[i];
    d[i] = b[i] + c[i];
    ia[i] = ib[i] + ic[i];
  }
  ibar (ia);
  fbar (a);
  fbar (d);

  /* Not vetorizable yet (too conservative dependence test).  */
  for (i = 0; i < N; i++){
    a[i] = b[i] + c[i];
    a[i+1] = b[i] + c[i];
  }
  fbar (a);
}

/* { dg-final { scan-tree-dump-times "vectorized 6 loops" 1 "vect" { target vect_strided2 } } } */
/* { dg-final { scan-tree-dump-times "vectorized 5 loops" 1 "vect" { xfail vect_strided2 } } } */
