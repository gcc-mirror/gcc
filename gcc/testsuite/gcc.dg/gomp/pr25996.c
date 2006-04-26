/* PR c/25996 */

void
test1 (void)
{
#pragma omp for
  for (i = 0; i < 1; ++i); /* { dg-error "undeclared|for each function" } */
}

void
test2 (void)
{
  int i;
#pragma omp for
  for (i = j; i < 1; ++i); /* { dg-error "undeclared" } */
}

void
test3 (void)
{
  int i;
#pragma omp for
  for (i = 0; i < j; ++i); /* { dg-error "undeclared|invalid controlling predicate" } */
}

void
test4 (void)
{
  int i;
#pragma omp for
  for (i = 0; i < 10; i += j); /* { dg-error "undeclared|invalid increment expression" } */
}
