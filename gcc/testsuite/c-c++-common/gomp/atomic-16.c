/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

int x = 6;

void
foo ()
{
  int v;
  #pragma omp atomic seq_cst read
  v = x;
  #pragma omp atomic seq_cst, read
  v = x;
  #pragma omp atomic seq_cst write
  x = v;
  #pragma omp atomic seq_cst ,write
  x = v;
  #pragma omp atomic seq_cst update
  x += v;
  #pragma omp atomic seq_cst , update
  x += v;
  #pragma omp atomic seq_cst capture
  v = x += 2;
  #pragma omp atomic seq_cst, capture
  v = x += 2;
  #pragma omp atomic read , seq_cst
  v = x;
  #pragma omp atomic write ,seq_cst
  x = v;
  #pragma omp atomic update, seq_cst
  x += v;
  #pragma omp atomic capture, seq_cst
  v = x += 2;
}
