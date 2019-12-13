// PR c++/91363 - P0960R3: Parenthesized initialization of aggregates.
// { dg-do compile { target c++11 } }

struct A { };
struct B { };
static_assert (!__is_trivially_constructible(A, B), "");
