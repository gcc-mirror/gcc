/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */

float x[2*256+1];

void foo(void)
{
  int i;
  for (i=0; i<256; ++i)
   {
    x[2*i] = x[2*i] * x[2*i];
    x[2*i+1] = x[2*i+1] * x[2*i+1];
   }
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { target vect_strided2 } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
