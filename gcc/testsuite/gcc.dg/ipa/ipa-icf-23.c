/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf"  } */

struct A
{
  int a;
  int b;
};

__attribute__ ((noinline))
int foo(struct A *a)
{
  return 123;
}

__attribute__ ((noinline))
int bar(struct A *b)
{
  return 123;
}

int main()
{
  return foo(0) + bar(0);
}

/* { dg-final { scan-ipa-dump "Semantic equality hit:foo->bar" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */
