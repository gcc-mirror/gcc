/* PR middle-end/99928 */

int v;

void
foo (void)
{
  #pragma omp target parallel firstprivate (v) map(tofrom: v)	/* { dg-bogus "'v' appears both in data and map clauses" } */
  v++;
}

void
bar (void)
{
  #pragma omp target firstprivate (v) map (tofrom: v)	/* { dg-error "'v' appears both in data and map clauses" } */
  v++;
}

void
baz (void)
{
  int j;
  #pragma omp target simd firstprivate (v) map (tofrom: v) private (j)	/* { dg-error "'v' appears both in data and map clauses" } */
  for (int i = 0; i < 1; i++)
    j = v;
}
