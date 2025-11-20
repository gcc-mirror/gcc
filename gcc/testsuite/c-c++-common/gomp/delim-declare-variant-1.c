/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

/* Check basic functionality for the delimited form of "declare variant"
   - no error re duplicate definitions
   - variants are registered and correctly resolved at call site.  */

int foo (int a)
{
  return a;
}

int bar (int x)
{
  return x;
}

#pragma omp begin declare variant match (construct={target})
int foo (int a)
{
  return a + 1;
}

int bar (int x)
{
  return x * 2;
}
#pragma omp end declare variant

/* Because of the high score value, this variant for "bar" should always be
   selected even when the one above also matches.  */
#pragma omp begin declare variant match (implementation={vendor(score(10000):"gnu")})
int bar (int x)
{
  return x * 4;
}
#pragma omp end declare variant

int main (void)
{
  if (foo (42) != 42) __builtin_abort ();
  if (bar (3) != 12) __builtin_abort ();
#pragma omp target
  {
    if (foo (42) != 43) __builtin_abort ();
    if (bar (3) != 12) __builtin_abort ();
  }
}

/* { dg-final { scan-tree-dump-times "omp declare variant base \\(foo.ompvariant." 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "omp declare variant base \\(bar.ompvariant." 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "foo \\(42\\)" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "foo\\.ompvariant. \\(42\\)" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "bar \\(3\\)" 0 "gimple" } } */
/* { dg-final { scan-tree-dump-times "bar\\.ompvariant. \\(3\\)" 2 "gimple" } } */
