/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf-optimized"  } */

__attribute__ ((noinline))
int foo(int x)
{
  int c = x;

  if (x > 10)
    c += 2;
  else
    c -= 3;

  return c;
}

__attribute__ ((noinline))
int bar(int y)
{
  int d = y;

  if (y > 10)
    d += 2;
  else
    d -= 3;

  return d;
}

int main()
{
  return 0;
}

/* { dg-final { scan-ipa-dump "Semantic equality hit:foo->bar" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */
