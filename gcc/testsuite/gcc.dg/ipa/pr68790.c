/* { dg-do run } */
/* { dg-options "-O0 -fipa-icf -fdump-ipa-icf"  } */

struct S
{
  int a;
};

int
foo3 (struct S x, struct S y, struct S z)
{
  if (z.a != 9)
    __builtin_abort ();
  return 0;
}

int
bar3 (struct S x, struct S y, struct S z)
{
  return foo3 (y, x, z);
}

int
baz3 (struct S x, struct S y, struct S z)
{
  return foo3 (y, z, x);
}

int
main (void)
{
  struct S
    a = { 3 },
    b = { 6 },
    c = { 9 };

  return bar3 (b, a, c);
}

/* { dg-final { scan-ipa-dump "Equal symbols: 0" "icf"  } } */
