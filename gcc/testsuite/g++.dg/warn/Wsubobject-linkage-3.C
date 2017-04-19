// PR c++/53184

namespace { struct Foo { }; }

#line 6 "foo.C"
struct Bar { Foo foo; };   // { dg-warning "anonymous namespace" }
// { dg-bogus "no linkage" "" { target *-*-* } .-1 }
struct Bar2 : Foo { };     // { dg-warning "anonymous namespace" }
// { dg-bogus "no linkage" "" { target *-*-* } .-1 }
