/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf -fno-inline"  } */

struct str
{
  unsigned a:1, b:1;
};

static struct str test;

unsigned foo(struct str *s)
{
  return s->a;
}

unsigned bar(struct str *s)
{
  return s->a;
}

int main()
{
  test.a = 0;
  test.b = 1;

  return foo (&test) == bar (&test);
}

/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */
