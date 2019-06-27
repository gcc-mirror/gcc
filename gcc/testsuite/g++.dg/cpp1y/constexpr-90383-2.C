// PR c++/90383
// { dg-do run { target c++14 } }
// { dg-options "-O2" }

extern "C" void abort ();
struct alignas(8) A { constexpr A (bool x) : a(x) {} A () = default; bool a; };
struct B { A b; };

constexpr bool
foo ()
{
  B w{A (true)};
  w.b = A (true);
  return w.b.a;
}

int
main ()
{
  if (!foo ())
    abort ();
}
