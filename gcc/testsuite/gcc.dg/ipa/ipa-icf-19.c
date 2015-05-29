/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf"  } */

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

  if (y > 11)
    d += 2;
  else
    d -= 3;

  return d;
}

int main()
{
  return 0;
}

/* { dg-final { scan-ipa-dump-not "Semantic equality hit:" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 0" "icf"  } } */
