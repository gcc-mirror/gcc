/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

/* Check "begin declare variant" on template functions.  */

template <typename T>
T foo (T a)
{
  return a;
}

template <typename T>
T bar (T x)
{
  return x;
}

#pragma omp begin declare variant match (construct={target})
template <typename T1>
T1 foo (T1 a)
{
  return a + 1;
}

template <typename T1>
T1 bar (T1 x)
{
  return x * 2;
}
#pragma omp end declare variant

/* Because of the high score value, this variant for "bar" should always be
   selected even when the one above also matches.  */
#pragma omp begin declare variant match (implementation={vendor(score(10000):"gnu")})
template <typename T2>
T2 bar (T2 x)
{
  return x * 4;
}
#pragma omp end declare variant

int main (void)
{
  if (foo<int> (42) != 42) __builtin_abort ();
  if (bar<int> (3) != 12) __builtin_abort ();
#pragma omp target
  {
    if (foo<int> (42) != 43) __builtin_abort ();
    if (bar<int> (3) != 12) __builtin_abort ();
  }
}

/* Make sure all the template functions are instantiated.  */
/* { dg-final { scan-tree-dump "int foo.ompvariant.<int> \\(.*\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "int foo<int> \\(.*\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "int bar.ompvariant.<int> \\(.*\\)" "gimple" } } */

/* Make sure the calls are resolved correctly.  */
/* { dg-final { scan-tree-dump-times "foo<int> \\(42\\)" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "foo\\.ompvariant.<int> \\(42\\)" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "bar<int> \\(3\\)" 0 "gimple" } } */
/* { dg-final { scan-tree-dump-times "bar\\.ompvariant.<int> \\(3\\)" 2 "gimple" } } */

/* The variants must have internal linkage, not .globl or .weak.  */
/* { dg-final { scan-assembler-not "\\.globl\[ \t\]*_?_Z15foo.ompvariant" } } */
/* { dg-final { scan-assembler-not "\\.globl\[ \t\]*_?_Z15bar.ompvariant" } } */
/* { dg-final { scan-assembler-not "\\.weak\[ \t\]*_?_Z15foo.ompvariant" } } */
/* { dg-final { scan-assembler-not "\\.weak\[ \t\]*_?_Z15bar.ompvariant" } } */




