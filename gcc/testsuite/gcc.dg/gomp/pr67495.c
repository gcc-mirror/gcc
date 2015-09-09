/* PR c/67495 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

int a, b, c;

void
foo (void)
{
#pragma omp atomic capture
  a = (float)a + b;	/* { dg-error "invalid operator" } */
#pragma omp atomic read
  (float) a = b;	/* { dg-error "lvalue required" } */
#pragma omp atomic write
  (float) a = b;	/* { dg-error "lvalue required" } */
#pragma omp atomic read
  a = (float) b;	/* { dg-error "lvalue required" } */
#pragma omp atomic capture
  (float) a = b += c;	/* { dg-error "lvalue required" } */
#pragma omp atomic capture
  { a += b; (float) c = a; }	/* { dg-error "lvalue required" } */
#pragma omp atomic capture
  { a += b; c = (float) a; }	/* { dg-error "uses two different expressions for memory" } */
#pragma omp atomic capture
  a = (int)a + b;	/* { dg-error "invalid operator" } */
#pragma omp atomic read
  (int) a = b;		/* { dg-error "lvalue required" } */
#pragma omp atomic write
  (int) a = b;		/* { dg-error "lvalue required" } */
#pragma omp atomic read
  a = (int) b;		/* { dg-error "lvalue required" } */
#pragma omp atomic capture
  (int) a = b += c;	/* { dg-error "lvalue required" } */
#pragma omp atomic capture
  { a += b; (int) c = a; }	/* { dg-error "lvalue required" } */
#pragma omp atomic capture
  { a += b; c = (int) a; }	/* { dg-error "lvalue required" } */
}
