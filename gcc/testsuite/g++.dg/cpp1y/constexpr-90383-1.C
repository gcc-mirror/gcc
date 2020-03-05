// PR c++/90383
// { dg-do compile { target c++14 } }

struct alignas(8) A { constexpr A (bool x) : a(x) {} A () = delete; bool a; };
struct B { A b; };

constexpr bool
foo ()
{
  B w{A (true)};
  w.b = A (true);
  return w.b.a;
}

static_assert (foo (), "");
