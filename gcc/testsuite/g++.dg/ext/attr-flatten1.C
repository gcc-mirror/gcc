// PR c++/96078
// { dg-do compile { target c++11 } }

struct A {
    [[gnu::flatten]] A() {}
    [[gnu::flatten]] ~A() {}
};

A a;
