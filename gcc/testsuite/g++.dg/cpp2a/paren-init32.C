// PR c++/92812
// P1975R0
// { dg-do compile { target c++20 } }

struct A { int i; };
struct A2 { int i; A2(int); };
struct A3 { int i; explicit A3(int); };

struct X { A a; };
auto x = static_cast<X>(42); // { dg-error "could not convert" }

struct X2 { A2 a; };
auto x2 = static_cast<X2>(42);

struct X3 { A3 a; };
// Aggregate-initialization copy-initializes, so the explicit ctor
// isn't considered.
auto x3 = static_cast<X3>(42); // { dg-error "could not convert" }

struct NonAggr { int i; virtual void foo (); };
auto x4 = static_cast<NonAggr>(42); // { dg-error "no matching" }
