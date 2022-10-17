// PR c++/103879
// { dg-do compile { target c++14 } }

struct A { int n = 42; };
struct Y { int m = 0; };
struct X : Y, A { };
struct B : X { };
struct C { B b; };

constexpr int f() {
  C c;
  A& a = static_cast<A&>(c.b);
  B& b = static_cast<B&>(a);
  return b.n;
}

static_assert(f() == 42, "");
