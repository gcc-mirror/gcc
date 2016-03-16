// PR c++/70259
// { dg-options -O2 }
// { dg-do run }

struct Empty { };
struct A { A() : a(true) { } bool a; };
struct B : Empty { B() : Empty() { } };
struct C : A, B { C() : A(), B() { } };
int main() {
  C c;
  if ( c.a == false )
    __builtin_abort();
};
