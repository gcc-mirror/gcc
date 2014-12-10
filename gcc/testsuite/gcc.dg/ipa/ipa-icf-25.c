/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf"  } */

static int zip();
static int zap();
static int two();

__attribute__ ((noinline))
int foo()
{
  return zip();
}

__attribute__ ((noinline))
int bar()
{
  return zap();
}

__attribute__ ((noinline))
int baz()
{
  return two();
}

__attribute__ ((noinline))
int zip()
{
  return 0;
}

__attribute__ ((noinline))
int zap()
{
  return 0;
}

__attribute__ ((noinline))
int two()
{
  return 2;
}

int main()
{
  return foo() + bar();
}

/* { dg-final { scan-ipa-dump "Semantic equality hit:bar->foo" "icf"  } } */
/* { dg-final { scan-ipa-dump "Semantic equality hit:zap->zip" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 2" "icf"  } } */
/* { dg-final { cleanup-ipa-dump "icf" } } */
