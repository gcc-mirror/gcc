// PR c++/127672
// { dg-do compile { target c++11 } }

namespace { struct P { }; }

#line 7 "foo.C"
struct S { void (*f) (P *); };	  // { dg-warning "uses the anonymous namespace" }
struct S2 { void (P::*m) (); };	  // { dg-warning "uses the anonymous namespace" }
struct S3 { P (*g) (); };	  // { dg-warning "uses the anonymous namespace" }
struct S4 { int P::*d; };	  // { dg-warning "uses the anonymous namespace" }
