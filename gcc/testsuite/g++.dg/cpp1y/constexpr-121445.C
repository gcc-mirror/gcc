// PR c++/121445
// { dg-do compile { target c++14 } }

struct D {
  const char *d;
  const char *foo () { return ""; };			// { dg-message "declared here" "" { target c++20_down } }
  constexpr D () : d (foo ()) {}			// { dg-error "call to non-'constexpr' function 'const char\\\* D::foo\\\(\\\)'" "" { target c++20_down } }
  constexpr D &operator= (const char *) { return *this; }
};
struct S {
  constexpr S ()
  {
    struct A { D a; };
    struct B { A b; };
    struct C { B c; };
    C d {};
    d.c.b.a = "";
  }
};
