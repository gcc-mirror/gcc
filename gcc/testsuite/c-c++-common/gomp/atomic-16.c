/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

int x = 6;

void
foo ()
{
  int v;
  #pragma omp atomic seq_cst load	/* { dg-error "expected end of line" } */
  v = x;			  	/* { dg-error "invalid form" } */
  #pragma omp atomic seq_cst, load	/* { dg-error "expected end of line" } */
  v = x;			  	/* { dg-error "invalid form" } */
  #pragma omp atomic seq_cst store	/* { dg-error "expected end of line" } */
  x = v;			  	/* { dg-error "invalid form" } */
  #pragma omp atomic seq_cst ,store	/* { dg-error "expected end of line" } */
  x = v;			  	/* { dg-error "invalid form" } */
  #pragma omp atomic seq_cst update	/* { dg-error "expected end of line" } */
  x += v;
  #pragma omp atomic seq_cst , update	/* { dg-error "expected end of line" } */
  x += v;
  #pragma omp atomic seq_cst capture	/* { dg-error "expected end of line" } */
  v = x += 2;			  	/* { dg-error "invalid form" } */
  #pragma omp atomic seq_cst, capture	/* { dg-error "expected end of line" } */
  v = x += 2;			  	/* { dg-error "invalid form" } */
  #pragma omp atomic load , seq_cst	/* { dg-error "expected end of line" } */
  v = x;			  	/* { dg-error "invalid form" } */
  #pragma omp atomic store ,seq_cst	/* { dg-error "expected end of line" } */
  x = v;			  	/* { dg-error "invalid form" } */
  #pragma omp atomic update, seq_cst	/* { dg-error "expected end of line" } */
  x += v;
  #pragma omp atomic capture, seq_cst	/* { dg-error "expected end of line" } */
  v = x += 2;
}
