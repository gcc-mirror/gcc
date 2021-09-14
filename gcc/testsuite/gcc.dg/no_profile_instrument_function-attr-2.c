/* { dg-require-effective-target global_constructor } */
/* { dg-options "-O2 -fprofile-generate -fprofile-update=single -fdump-tree-optimized" } */

__attribute__ ((no_profile_instrument_function))
int foo()
{
  return 0;
}

int bar()
{
  return foo();
}

/* { dg-final { scan-tree-dump-not " = foo \\(\\)" "optimized"} } */
