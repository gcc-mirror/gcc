// PR c++/50608
// Testcase by <dberger@oubliette.org>
// { dg-do compile }

struct A {
    int offset;
};

struct B: public A {
};

struct C {
    A a;
    B b;
};

int fails = __builtin_offsetof (C, b.offset);
