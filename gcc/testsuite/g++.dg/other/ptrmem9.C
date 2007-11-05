// PR c++/33969
// { dg-do compile }

struct A;
void (*A::* fp)() const; // { dg-error "invalid in variable declaration" }
