// PR c++/89511
// { dg-do compile { target c++11 } }

void f ()
{
  enum e { a };
  using e::a; // { dg-error "not a namespace or unscoped enum" }
}

struct S {
  enum E { A };
  using E::A; // { dg-error "type .S. is not a base type for type .S." }
};

namespace N {
  enum E { B };
}

struct T {
  using N::E::B; // { dg-error "using-declaration for non-member at class scope" }
};
