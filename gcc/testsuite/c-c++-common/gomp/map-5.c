/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

void foo (void)
{
  /* Basic test to ensure to,from,tofrom is ordered before alloc,release,delete clauses.  */
  int a, b, c;
  #pragma omp target enter data map(alloc:a) map(to:b) map(alloc:c)
  #pragma omp target exit data map(from:a) map(release:b) map(from:c)

  #pragma omp target map(alloc:a) map(tofrom:b) map(alloc:c)
  a = b = c = 1;

  #pragma omp target enter data map(to:a) map(alloc:b) map(to:c)
  #pragma omp target exit data map(from:a) map(delete:b) map(from:c)
}

/* { dg-final { scan-tree-dump "pragma omp target enter data map\\(to:.* map\\(alloc:.* map\\(alloc:.*" "gimple" } } */
/* { dg-final { scan-tree-dump "pragma omp target exit data map\\(from:.* map\\(from:.* map\\(release:.*" "gimple" } } */

/* { dg-final { scan-tree-dump "pragma omp target num_teams.* map\\(tofrom:.* map\\(alloc:.* map\\(alloc:.*" "gimple" } } */

/* { dg-final { scan-tree-dump "pragma omp target enter data map\\(to:.* map\\(to:.* map\\(alloc:.*" "gimple" } } */
/* { dg-final { scan-tree-dump "pragma omp target exit data map\\(from:.* map\\(from:.* map\\(delete:.*" "gimple" } } */
