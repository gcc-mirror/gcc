// PR c++/58126

struct A {
  const int value1;
  int& value2;
};

struct B : A { };

A a;  // { dg-error "uninitialized const member in 'struct A'|uninitialized reference member in 'struct A'" }

B b;  // { dg-error "uninitialized const member in base 'struct A' of 'struct B'|uninitialized reference member in base 'struct A' of 'struct B'" }
