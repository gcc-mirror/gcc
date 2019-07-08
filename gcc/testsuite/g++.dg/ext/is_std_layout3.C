// DR 1813
// PR c++/83374 - __is_standard_layout wrong for a class with repeated bases.
// { dg-do compile { target c++11 } }

struct B { int i; };        // standard-layout class
struct C : B { };           // standard-layout class
struct D : C { };           // standard-layout class
struct E : D { char : 4; }; // not a standard-layout class
static_assert( __is_standard_layout(B), "" );
static_assert( __is_standard_layout(C), "" );
static_assert( __is_standard_layout(D), "" );
static_assert( ! __is_standard_layout(E), "" );

struct Q {};
struct S : Q { };
struct T : Q { };
struct U : S, T { };         // not a standard-layout class
static_assert( ! __is_standard_layout(U), "" );
