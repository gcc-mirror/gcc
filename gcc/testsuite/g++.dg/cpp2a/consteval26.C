// PR c++/103912
// { dg-do run { target c++20 } }
// { dg-additional-options "-O2 -g -fkeep-inline-functions" }

extern "C" void abort ();

struct A { A () {} };

consteval auto
foo ()
{
  if (1)
    ;
  return [] (A x) { return 1; };
}

consteval auto
bar (int a)
{
  int b = a + 4;
  if (1)
    ;
  return [=] (A x) { return a + b; };
}

int
main ()
{
  A x;
  auto h = foo ();
  if (h (x) != 1)
    abort ();
  auto i = bar (5);
  if (i (x) != 14)
    abort ();
  auto j = bar (42);
  if (j (x) != 88)
    abort ();
}
