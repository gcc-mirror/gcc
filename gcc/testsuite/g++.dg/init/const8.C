// DR 234 - it should be OK to leave off the initializer of a const
// variable if the default constructor fully initializes the object.

struct A { };
const A a;

struct B { A a; };
const B b;

struct C { virtual void f(); };
const C c;
