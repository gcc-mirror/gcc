// PR c++/55643
// { dg-do compile }
// { dg-options "-std=c++11 -Wunused" }

enum class E { e = 123 };

int
foo ()
{
  E x = E::e;
  return (double) x;
}

int
bar ()
{
  E x = E::e;
  return (long double) x;
}

int
baz ()
{
  E x = E::e;
  return (float) x;
}
