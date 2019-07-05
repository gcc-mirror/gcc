// DR 1813
// PR c++/83374 - __is_standard_layout wrong for a class with repeated bases.
// { dg-do compile { target c++11 } }

struct R { };
struct Q { };
struct S : R { };
struct T : Q { };
struct U : S, T { };
// No repeated base class subobjects.
static_assert(__is_standard_layout(U), "");
