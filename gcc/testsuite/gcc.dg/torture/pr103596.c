/* { dg-do compile } */
/* { dg-additional-options "--param case-values-threshold=1" } */

int n;

void
qux (int a)
{
}

int
baz (void)
{
  return -1;
}

__attribute__ ((returns_twice)) int
bar (int b)
{
  if (n != 0)
    {
      if (b != 2)
        if (b != 0)
          return n + b;

      if (n == 2)
        return 0;
    }
}

void
foo (void)
{
  qux (n);
  bar (baz ());
}
