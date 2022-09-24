/* PR c/106981 */
/* { dg-do compile } */

void
foo (int a, double *b, double *c, double *d, long long e)
{
#pragma omp atomic capture
  c[a] = d[((int) (e / 10 + 1))] = b[a] + d[((int) e / 10 + 1)];	/* { dg-error "invalid form" } */
}
