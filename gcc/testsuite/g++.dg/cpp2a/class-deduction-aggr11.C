// PR c++/101344
// { dg-do compile { target c++20 } }

template<class T=void>
struct A { int m; int t[2]; };

A a1{1, {2, 3}}; // previously rejected
A a2{1, 2, 3};

struct B { int x, y; };

template<class T=void>
struct C { int m; struct { int x, y; } t; };

A b1{1, {2, 3}}; // previously rejected
A b2{1, 2, 3};

template<class T>
struct D { T t[2]; };

D d1{1, 2};
D d2{{1, 2}}; // previously rejected

template<class T>
struct E { T t[2][2]; };

E e1{1, 2, 3, 4};
E e2{{{1, 2}, {3, 4}}}; // previously rejected
E e3{{1, 2, 3, 4}}; // { dg-error "deduction|no match" }
