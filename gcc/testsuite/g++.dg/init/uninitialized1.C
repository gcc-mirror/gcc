// PR c++/58126

struct A { // { dg-error "uninitialized" "" { target c++11 } }
  const int value1; // { dg-message "should be initialized" }
  int& value2; // { dg-message "should be initialized" }
};

struct B : A { };	// { dg-error "deleted" "" { target c++11 } }

A a;  // { dg-error "deleted|uninitialized const member in 'struct A'|uninitialized reference member in 'struct A'" }

B b;  // { dg-error "deleted|uninitialized const member in base 'struct A' of 'struct B'|uninitialized reference member in base 'struct A' of 'struct B'" }
