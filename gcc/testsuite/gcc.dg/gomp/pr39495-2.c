/* PR c/39495 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

#define INT_MIN (-__INT_MAX__ - 1)
#define INT_MAX __INT_MAX__
#define UINT_MAX (2U * __INT_MAX__ + 1)

int
foo (void)
{
  int i;
  unsigned int u;

#pragma omp for
  for (i = INT_MIN + 6; i != INT_MIN; i--)	/* { dg-error "invalid controlling predicate" } */
    ;
#pragma omp for
  for (i = INT_MIN + 6; i == INT_MIN; i--)	/* { dg-error "invalid controlling predicate" } */
    ;
#pragma omp for
  for (i = INT_MAX - 6; i != INT_MAX; i++)	/* { dg-error "invalid controlling predicate" } */
    ;
#pragma omp for
  for (i = INT_MAX - 6; i == INT_MAX; i++)	/* { dg-error "invalid controlling predicate" } */
    ;
#pragma omp for
  for (u = 6; u != 0; u--)			/* { dg-error "invalid controlling predicate" } */
    ;
#pragma omp for
  for (u = 6; u == 0; u--)			/* { dg-error "invalid controlling predicate" } */
    ;
#pragma omp for
  for (u = UINT_MAX - 6; u != UINT_MAX; u++)	/* { dg-error "invalid controlling predicate" } */
    ;
#pragma omp for
  for (u = UINT_MAX - 6; u == UINT_MAX; u++)	/* { dg-error "invalid controlling predicate" } */
    ;
}
