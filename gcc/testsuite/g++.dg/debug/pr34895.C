// { dg-do compile }
// { dg-options "-O2 -g" }
//
// Copyright (C) 2008 Free Software Foundation, Inc.
// Contributed by Theodore.Papadopoulo 20 Jan 2008 <Theodore.Papadopoulo@sophia.inria.fr>

struct A {
    A() { }
    unsigned operator()() { return 1; }
};
struct B: public A {
    typedef const A base;
    using base::operator();
    B() { }
};
int
main() {
    B b;
}
