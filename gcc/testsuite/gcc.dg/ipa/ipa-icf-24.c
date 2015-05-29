/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf"  } */

struct A
{
  int a, b, c, d;
};

struct B
{
  int x, y, z;
};

__attribute__ ((noinline))
int foo(struct A *a)
{
  a->c = 1;

  return 123;
}

__attribute__ ((noinline))
int bar(struct B *b)
{
  b->z = 1;

  return 123;
}

int main()
{
  return foo(0) + bar(0);
}

/* { dg-final { scan-ipa-dump "Equal symbols: 0" "icf"  } } */
