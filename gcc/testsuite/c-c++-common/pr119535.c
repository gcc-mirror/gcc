/* PR gcov-profile/119535
/* { dg-do compile { target musttail } } */
/* { dg-options "-fprofile-generate -O2" } */
/* { dg-require-profiling "-fprofile-generate" } */

[[gnu::noipa]] int
foo (int x)
{
  return 42 + x;
}

int
bar (int x)
{
  foo (x);
  foo (2);
  [[clang::musttail]] return foo (3);
}

int
baz (int x)
{
  if (x == 42)
    return -1;
  else if (x == 15)
    return 25;
  else if (x == 26)
    [[clang::musttail]] return foo (4);
  else
    [[clang::musttail]] return foo (5);
}
