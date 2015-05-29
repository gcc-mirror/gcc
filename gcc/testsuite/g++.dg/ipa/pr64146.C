/* { dg-do compile } */
/* { dg-require-alias "" } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fpic -fdump-ipa-icf-details -fipa-icf"  } */

extern "C" const char*
foo()
{
  return "original";
}

const char*
test_foo()
{
  return foo();
}

extern "C" const char*
bar()
{
  return "original";
}

const char*
test_bar()
{
  return bar();
}

int main (int argc, char **argv)
{
  test_foo ();
  test_bar ();

  return 0;
}

/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */
