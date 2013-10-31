// PR c++/58162
// { dg-require-effective-target c++11 }

struct A {
 A();
 A(A&&);
};

struct B {
 A const a = A();
};

B b;
