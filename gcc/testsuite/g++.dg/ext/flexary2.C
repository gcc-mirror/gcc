// PR c++/46688
// { dg-options "" }

struct A {
   A(int);
};

struct B {
   B() {}
   A a[];
};
