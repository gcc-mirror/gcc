//PR c++/27494

struct A
{
    template<operator+> void foo() {}   // { dg-error "14:declaration" }
  // { dg-error "expected|template" "" { target *-*-* } .-1 }
};
