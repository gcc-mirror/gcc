/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf"  } */

__attribute__ ((noinline))
int foo(int a)
{
  return a * a;
}

__attribute__ ((noinline))
int bar(int b)
{
  return b;
}

__attribute__ ((noinline))
void caller(int x)
{
  return;
}

int main(int argc, char **argv)
{
  caller(foo(argc));
  caller(bar(argc));

  return 123;
}

/* { dg-final { scan-ipa-dump-not "Semantic equality hit:" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 0" "icf"  } } */
