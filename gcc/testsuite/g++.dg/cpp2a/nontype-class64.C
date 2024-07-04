// Testcase from P2308R1
// { dg-do compile { target c++20 } }

template<auto n> struct B { /* ... */ };
B<5> b1;                        // OK, template parameter type is int
B<'a'> b2;                      // OK, template parameter type is char
B<2.5> b3;                      // OK, template parameter type is double
B<void(0)> b4;		       // { dg-error "void" }

template<int i> struct C { /* ... */ };
C<{ 42 }> c1;  // OK

struct J1 {
  J1 *self=this;
};
B<J1{}> j1;  // { dg-error "not a constant expression" }

struct J2 {
  J2 *self=this;
  constexpr J2() {}
  constexpr J2(const J2&) {}
};
B<J2{}> j2;  // { dg-error "" }
