/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

#define N 128

int out[N*4], out2[N], in[N*4];

void
foo ()
{
  int i, a0, a1, a2, a3;

  for (i = 0; i < N; i++) 
    { 
      a0 = in[i*4];
      a1 = in[i*4 + 1];
      a2 = in[i*4 + 2];
      a3 = in[i*4 + 3];

      out[i*4] = a0;
      out[i*4 + 1] = a1;
      out[i*4 + 2] = a2;
      out[i*4 + 3] = a3;

      out2[i] = a0;
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target vect_strided4 } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect" { target vect_strided4 } } } */
  
