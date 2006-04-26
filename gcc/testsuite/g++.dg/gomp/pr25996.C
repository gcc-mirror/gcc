// PR c/25996

void
test1 (void)
{
#pragma omp for
  for (i = 0; i < 1; ++i); // { dg-error "not declared|expected iteration decl" }
}

void
test2 (void)
{
  int i;
#pragma omp for
  for (i = j; i < 1; ++i); // { dg-error "not declared|expected iteration decl" }
}

void
test3 (void)
{
  int i;
#pragma omp for
  for (i = 0; i < j; ++i); // { dg-error "not declared|invalid controlling predicate" }
}

void
test4 (void)
{
  int i;
#pragma omp for
  for (i = 0; i < 10; i += j); // { dg-error "not declared|invalid increment expression" }
}
