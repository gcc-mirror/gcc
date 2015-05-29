/* { dg-do compile } */
/* { dg-require-alias "" } */
/* { dg-options "-O0 -fipa-icf -fdump-ipa-icf"  } */

static int do_work(void)
{
  return 0;
}

static int foo() __attribute__((alias("do_work")));
static int bar() __attribute__((alias("do_work")));

static int a()
{
  return foo();
}

static int b()
{
  return bar();
}

int main()
{
  return a() + b();
}

/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */
