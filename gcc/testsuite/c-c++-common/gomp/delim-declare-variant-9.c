/* { dg-do compile } */

/* Check diagnostics for mismatched pragma/attribute forms of delimited
   declare variant.  */

int foo (int a)
{
  return a;
}

int bar (int x)
{
  return x;
}

[[omp::directive (begin declare variant match (construct={target}))]];
int foo (int a)
{
  return a + 1;
}

int bar (int x)
{
  return x * 2;
}
#pragma omp end declare variant  /* { dg-error "'begin declare variant' in attribute syntax terminated with 'end declare variant' in pragma syntax" } */

/* Because of the high score value, this variant for "bar" should always be
   selected even when the one above also matches.  */
#pragma omp begin declare variant match (implementation={vendor(score(10000):"gnu")})
int bar (int x)
{
  return x * 4;
}
[[omp::directive (end declare variant)]];  /* { dg-error "'begin declare variant' in pragma syntax terminated with 'end declare variant' in attribute syntax" } */

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

