// PR c++/53184

typedef volatile struct { } Foo;

#line 6 "foo.C"
struct Bar { Foo foo; };   // { dg-warning "no linkage" }
// { dg-bogus "anonymous namespace" "" { target *-*-* } .-1 }
struct Bar2 : Foo { };     // { dg-warning "no linkage" }
// { dg-bogus "anonymous namespace" "" { target *-*-* } .-1 }
