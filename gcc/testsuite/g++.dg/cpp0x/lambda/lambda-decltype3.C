// PR c++/79620
// [expr.prim.id.unqual] example 1
// { dg-do compile { target c++11 } }

void f() {
  float x, &r = x;

  [=]() -> decltype((x)) {      // lambda returns float const& because this lambda is not mutable and
                                // x is an lvalue
    decltype(x) y1;             // y1 has type float
    decltype((x)) y2 = y1;      // y2 has type float const&
    decltype(r) r1 = y1;        // r1 has type float&
    decltype((r)) r2 = y2;      // r2 has type float const&
    return y2;                  // { dg-bogus "'float&' to 'const float'" "" { xfail *-*-* } }
  };

  [=](decltype((x)) y) {
    decltype((x)) z = x;        // OK, y has type float&, z has type float const&
  };

  [=] {
    [](decltype((x)) y) {};     // OK, lambda takes a parameter of type float const&

#if __cpp_init_captures
    [x=1](decltype((x)) y) {
      decltype((x)) z = x;      // OK, y has type int&, z has type int const&
    };
#endif
  };
}
