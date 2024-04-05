// PR c++/91079
// DR 1881 - Standard-layout classes and unnamed bit-fields
// { dg-do compile { target c++11 } }

struct A { int a : 4; };
struct B : A { int b : 3; };
static_assert(__is_standard_layout(A), "");
static_assert(!__is_standard_layout(B), "");

struct C { int : 0; };
struct D : C { int : 0; };
static_assert(__is_standard_layout(C), "");
static_assert(!__is_standard_layout(D), "");
