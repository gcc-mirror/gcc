// C++26 P1306R5 - Expansion statements
// { dg-do compile { target c++11 } }
// { dg-options "-fno-implicit-constexpr" }

struct A { int s; };
constexpr A a[] = { { 3 }, { 4 }, { 5 }, { 6 }, { 7 } };
struct B {
  constexpr const A *begin () const { return &a[0]; }
  constexpr const A *end () const { return &a[s]; }
  int s;
};
constexpr B b = { 3 };
struct C {
  C (int x) : s (x) {}
  constexpr const A *begin () const { return &a[0]; }
  constexpr const A *end () const { return &a[s]; }
  int s;
};
struct D {
  constexpr D (int x) : s (x) {}
  constexpr const A *begin () const { return &a[0]; }
  const A *end () const { return &a[s]; }
  int s;
};
struct E {
  constexpr E (int x) : s (x) {}
  const A *begin () const { return &a[0]; }
  constexpr const A *end () const { return &a[s]; }
  int s;
};
struct F { F () : s (42) {} F (int x) : s (x) {} int s; };
F g[] = { { 3 }, { 4 }, { 5 }, { 6 }, { 7 } };
struct G {
  constexpr G (int x) : s (x) {}
  constexpr F *begin () const { return &g[0]; }
  constexpr F *end () const { return &g[s]; }
  int s;
};
struct H { int a; F b; int c; };

void
foo ()
{
  B c = { 3 };
  template for (constexpr auto g : c)	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;					// { dg-error "'c' is not a constant expression" "" { target *-*-* } .-1 }
  C d = { 3 };
  template for (constexpr auto g : d)	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;					// { dg-error "'d' is not a constant expression" "" { target *-*-* } .-1 }
					// { dg-error "call to non-'constexpr' function 'const A\\\* C::begin\\\(\\\) const'" "" { target c++11_down } .-1 }
					// { dg-error "call to non-'constexpr' function 'const A\\\* C::end\\\(\\\) const'" "" { target c++11_down } .-2 }
  constexpr D e = { 3 };
  template for (constexpr auto g : e)	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;					// { dg-error "'e' is not a constant expression" "" { target *-*-* } .-1 }
					// { dg-error "call to non-'constexpr' function 'const A\\\* D::end\\\(\\\) const'" "" { target *-*-* } .-1 }
  constexpr E f = { 3 };
  template for (constexpr auto g : f)	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;					// { dg-error "'f' is not a constant expression" "" { target *-*-* } .-1 }
					// { dg-error "call to non-'constexpr' function 'const A\\\* E::begin\\\(\\\) const'" "" { target *-*-* } .-1 }
  constexpr G h = { 3 };
  template for (constexpr auto g : h)	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;					// { dg-error "'h' is not a constant expression" "" { target *-*-* } .-1 }
  template for (constexpr auto g : { 1, 2, F { 3 }, 4L })	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;					// { dg-error "the type 'const F' of 'constexpr' variable 'g' is not literal" "" { target *-*-* } .-1 }
  template for (constexpr auto g : H {})// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;					// { dg-error "the type 'const F' of 'constexpr' variable 'g' is not literal" "" { target *-*-* } .-1 }
					// { dg-error "call to non-'constexpr' function 'F::F\\\(\\\)'" "" { target *-*-* } .-2 }
}
