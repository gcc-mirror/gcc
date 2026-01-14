// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct C {
  union {
    int i;
  };
};

struct D { int i; };

auto c = C{.i=2};
auto v = c.[:^^C::i:];    // { dg-error "not a base" }
auto e = c.[: ^^D::i :];  // { dg-error "not a base" }
