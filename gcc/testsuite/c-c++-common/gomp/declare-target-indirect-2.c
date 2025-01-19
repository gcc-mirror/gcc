/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

#pragma omp begin declare target indirect
void fn1 (void) { }
#pragma omp end declare target
/* { dg-final { scan-tree-dump "__attribute__\\\(\\\(omp declare target, omp declare target indirect\\\)\\\)\\\nvoid fn1" "gimple" } } */

#pragma omp begin declare target indirect (0)
void fn2 (void) { }
#pragma omp end declare target
/* { dg-final { scan-tree-dump "__attribute__\\\(\\\(omp declare target\\\)\\\)\\\nvoid fn2" "gimple" } } */

void fn3 (void) { }
#pragma omp declare target indirect to (fn3)
/* { dg-final { scan-tree-dump "__attribute__\\\(\\\(omp declare target indirect, omp declare target\\\)\\\)\\\nvoid fn3" "gimple" } } */

void fn4 (void) { }
#pragma omp declare target indirect (0) to (fn4)
/* { dg-final { scan-tree-dump "__attribute__\\\(\\\(omp declare target\\\)\\\)\\\nvoid fn4" "gimple" } } */

#pragma omp begin declare target indirect(1)
  int foo(void) { return 5; }
  #pragma omp begin declare target indirect(0)
    int bar(void) { return 8; }
    int baz(void) { return 11; }
    #pragma omp declare target indirect enter(baz)
  #pragma omp end declare target
#pragma omp end declare target
/* { dg-final { scan-tree-dump "__attribute__\\\(\\\(omp declare target, omp declare target indirect\\\)\\\)\\\nint foo" "gimple" } } */
/* { dg-final { scan-tree-dump "__attribute__\\\(\\\(omp declare target\\\)\\\)\\\nint bar" "gimple" } } */
/* { dg-final { scan-tree-dump "__attribute__\\\(\\\(omp declare target indirect, omp declare target\\\)\\\)\\\nint baz" "gimple" } } */
