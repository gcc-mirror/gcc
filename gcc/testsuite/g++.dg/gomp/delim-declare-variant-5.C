/* { dg-do compile } */

/* Check that "omp begin declare variant" for a namespace function outside of
   the namespace gives an error.  C++ generally does not allow injection of
   additional function into a namespace outside of its scope so this is just a
   generic error.  */

namespace n1 {

int foo (int a)
{
  return a;
}

int bar (int x)
{
  return x;
}

} /* namespace n1 */


#pragma omp begin declare variant match (construct={target})
int n1::foo (int a)  /* { dg-message "sorry, unimplemented: cannot handle qualified name for variant function" } */
{
  return a + 1;
}

int n1::bar (int x)  /* { dg-message "sorry, unimplemented: cannot handle qualified name for variant function" } */
{
  return x * 2;
}
#pragma omp end declare variant

/* Because of the high score value, this variant for "bar" should always be
   selected even when the one above also matches.  */
#pragma omp begin declare variant match (implementation={vendor(score(10000):"gnu")})
int n1::bar (int x)  /* { dg-message "sorry, unimplemented: cannot handle qualified name for variant function" } */
{
  return x * 4;
}
#pragma omp end declare variant

int main (void)
{
  if (n1::foo (42) != 42) __builtin_abort ();
  if (n1::bar (3) != 12) __builtin_abort ();
#pragma omp target
  {
    if (n1::foo (42) != 43) __builtin_abort ();
    if (n1::bar (3) != 12) __builtin_abort ();
  }
}
