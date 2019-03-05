// PR c++/88865
// { dg-do compile { target c++11 } }

struct B {};
struct A {
    [[no_unique_address]] B a;
    [[no_unique_address]] B b;
    [[no_unique_address]] B c;
    [[no_unique_address]] B d;
};

#define SA(X) static_assert((X),#X)
SA(sizeof(A) == 4);

A a;
SA(&a.a != &a.b);
SA(&a.c != &a.b);
SA(&a.c != &a.d);
