// PR c++/59645

struct A { virtual ~A(); };
struct B { virtual ~B(); };
struct C : A, B {};

struct X
{
  virtual B* foo(volatile int); // { dg-warning "deprecated" "" { target c++2a } }
};

struct Y : X
{
  virtual C* foo(volatile int); // { dg-warning "deprecated" "" { target c++2a } }
};

C* Y::foo(volatile int) { return 0; } // { dg-warning "deprecated" "" { target c++2a } }
