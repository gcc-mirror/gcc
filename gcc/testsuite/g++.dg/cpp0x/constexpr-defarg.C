// PR c++/46335
// { dg-do compile { target c++11 } }

struct T { };
struct A {
    A(const T &tr =T()) {}
};
struct B {
    A k;
};
B kk_;
A fk_;
