// { dg-options "-fkeep-inline-functions" }

struct B1 { virtual ~B1(); };
struct B2 { virtual ~B2(); };
struct D : B1, B2 {};
struct X : D      { X (); };

X::X () { new int; }
