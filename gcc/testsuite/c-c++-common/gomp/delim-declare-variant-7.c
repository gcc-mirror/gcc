/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

/* Test that merging of context selectors from an enclosing "begin declare
   variant" directive applies to nested regular "declare variant" directives
   (not just nested "begin declare variant", which is tested elsewhere).  */

extern int foo1 (int);
extern int foo2 (int);

#pragma omp begin declare variant match (implementation={vendor(gnu)})

#pragma omp declare variant (foo1) \
  match (construct={parallel,for})
#pragma omp declare variant (foo2) \
  match (device={kind(any)})
extern int foo (int);

#pragma omp end declare variant

int foo (int x)
{
  return x + 42;
}

/* { dg-final { scan-tree-dump-times "omp declare variant base" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "vendor \\(.gnu.\\)" 2 "gimple" } } */
