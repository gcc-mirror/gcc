/* { dg-do compile } */
/* { dg-options "-O1 -fharden-compares -fno-ipa-pure-const" } */

int
baz (void);

__attribute__ ((returns_twice)) void
bar (void)
{
}

int
quux (int y, int z)
{
  return (y || z >= 0) ? y : z;
}

int
foo (int x)
{
  int a = 0, b = x == a;

  bar ();

  if (!!baz () < quux (b, a))
    ++x;

  return x;
}
