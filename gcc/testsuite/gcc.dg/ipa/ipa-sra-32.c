/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-sra"  } */

/* Test that parameters can be removed even when they are returned but the
   return is unused.  */

extern int use(int use);


static int __attribute__((noinline))
foo(int a, int b, int c)
{
  use (c);
  return a + b + c;
}

static int __attribute__((noinline))
bar (int a, int b, int c, int d)
{
  return foo (a, b, c + d);
}

int
baz (int a, int b, int c, int d)
{
  bar (a, b, c, d);
  return a + d;
}

/* { dg-final { scan-ipa-dump-times "Will remove parameter" 4 "sra" } } */
