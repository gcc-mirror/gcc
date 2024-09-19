// PR c++/89511
// { dg-do compile { target c++11 } }

// [namespace.udecl] In a using-declaration used as a
// member-declaration, the nested-name-specifier shall name a base
// class of the class being defined
// (this changes in C++2a)

void f ()
{
  enum e { a };
  using e::a;  // { dg-bogus "redeclaration" "P1787" }
  // { dg-error "enum" "" { target { ! c++2a } } .-1 }
}

enum E { A };

struct S {
  enum E { A };
  using E::A; // { dg-error "not a base" "" { target { ! c++2a } } }
  // { dg-error "conflicts" "" { target c++2a } .-1 }
};

namespace N {
  enum E { B };
}

struct T {
  using N::E::B; // { dg-error "enum" "" { target { ! c++2a } } }
};
