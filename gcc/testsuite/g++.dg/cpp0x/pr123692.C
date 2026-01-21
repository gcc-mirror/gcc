// PR c++/123692
// { dg-do compile { target c++11 } }

struct A { A (int); bool operator == (A); };
struct B : A { using A::A; };
B operator - (B, B);
extern B c;
extern A d;
decltype (d == c - 1) x;
