// C++26 P1306R5 - Expansion statements
// { dg-do compile { target c++14 } }
// { dg-options "" }

struct A
{
  int x;
  constexpr explicit A (int v) : x(v) {}
  constexpr A &operator ++ () { ++x; return *this; }
  constexpr int operator * () const { return x; }
  constexpr bool operator != (const A &o) const { return x != o.x; }
  constexpr A operator + (int o) const { A r (x + o); return r; }
};

namespace N
{
  struct B { constexpr B () {} };
  constexpr A begin (B &) { return A (0); }
  constexpr A end (B &) { return A (6); }
}

void
foo ()
{
  template for (auto i : N::B {})				// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;								// { dg-error "no match for 'operator-' in '__for_begin  - __for_begin ' \\\(operand types are 'const A' and 'const A'\\\)" "" { target *-*-* } .-1 }
}
