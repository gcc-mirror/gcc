// PR c++/126508
// { dg-do compile { target c++26 } }

constexpr int
foo ()
{
  return __builtin_uncaught_exceptions ();
}

constexpr int
bar ()
{
  return __builtin_uncaught_exceptions ();
}

struct A { constexpr A () : a (0) {} constexpr ~A () { if (foo () != a) asm (""); } int a; };
struct B { constexpr B () : b (0) {} constexpr ~B () { if (bar () != b) asm (""); } int b; };

constexpr bool
baz ()
{
  {
    A a;
  }
  try
    {
      A a;
      B b;
      a.a = 1;
      b.b = 1;
      throw 42;
    }
  catch (...)
    {
    }
  {
    A a;
    B b;
  }
  return true;
}

static_assert (baz ());
