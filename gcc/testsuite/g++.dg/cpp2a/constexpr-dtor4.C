// PR c++/92414
// { dg-do compile { target c++2a } }

struct A { virtual void foo (); };

struct B : A {
  constexpr B (int);	// { dg-warning "used but never defined" }
  constexpr ~B () { }
};

struct D : B {
  constexpr D () : B (42) { }	// { dg-error "used before its definition" }
};

constexpr D d;	// { dg-message "in 'constexpr' expansion of" }
