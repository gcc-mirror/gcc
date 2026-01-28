// C++26 P1306R5 - Expansion statements
// { dg-do compile { target c++14 } }
// { dg-options "" }

struct S { int a; long b; short c; };
struct A
{
  int x;
  constexpr explicit A (int v) : x(v) {}
  constexpr A &operator ++ () { ++x; return *this; }
  constexpr int operator * () const { return x; }
  constexpr bool operator != (const A &o) const { return x != o.x; }
  constexpr A operator + (int o) const { A r (x + o); return r; }
  constexpr int operator - (const A &o) const { return x - o.x; }
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
  template for (int a = 1; auto a : { 1, 2L })		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;							// { dg-error "conflicting declaration 'auto a'" "" { target *-*-* } .-1 }
  template for (int b = 1; auto c : { 1, 2L })		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;
  int b = 1;
  int c = 2;
  template for (int d = 1; auto d : {})			// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;							// { dg-error "conflicting declaration 'auto d'" "" { target *-*-* } .-1 }
  template for (int e = 1; auto e : N::B {})		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;							// { dg-error "conflicting declaration 'auto e'" "" { target *-*-* } .-1 }
  template for (int f = 1; auto f : S { 1, 2, 3})	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;							// { dg-error "conflicting declaration 'auto f'" "" { target *-*-* } .-1 }
  template for (auto g : { 1, 2LL })			// { dg-warning "'template for' only available with" "" { target c++23_down } }
    int g = 5;						// { dg-error "conflicting declaration 'int g'" }
							// { dg-error "redeclaration of 'int g'" "" { target *-*-* } .-1 }
  template for (auto h : N::B {})			// { dg-warning "'template for' only available with" "" { target c++23_down } }
    int h = 6;						// { dg-error "redeclaration of 'int h'" }
  template for (auto i : S { 1, 2, 3})			// { dg-warning "'template for' only available with" "" { target c++23_down } }
    int i = 7;						// { dg-error "conflicting declaration 'int i'" }
							// { dg-error "redeclaration of 'int i'" "" { target *-*-* } .-1 }
  template for (auto j : { 1, 2LL })			// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      int j = 5;					// { dg-error "conflicting declaration 'int j'" }
    }							// { dg-error "redeclaration of 'int j'" "" { target *-*-* } .-1 }
  template for (auto k : N::B {})			// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      int k = 6;					// { dg-error "redeclaration of 'int k'" }
    }
  template for (auto l : S { 1, 2, 3})			// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      int l = 7;					// { dg-error "conflicting declaration 'int l'" }
    }							// { dg-error "redeclaration of 'int l'" "" { target *-*-* } .-1 }
}
