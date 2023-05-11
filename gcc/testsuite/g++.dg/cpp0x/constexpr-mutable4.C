// PR c++/109745
// Similar to constexpr-mutable1.C, but with nested 'mutable' accesses.
// { dg-do compile { target c++11 } }

struct A { mutable int m = 0; };

struct B { A a; };

struct C { B b; };

constexpr B b;
constexpr int bam = b.a.m;    // { dg-error "mutable" }

constexpr C c;
constexpr int cbam = c.b.a.m; // { dg-error "mutable" }
