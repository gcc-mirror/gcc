// PR c++/59645

struct A { virtual ~A(); };
struct B { virtual ~B(); };
struct C : A, B {};

struct X
{
  virtual B* foo(volatile int);
};

struct Y : X
{
  virtual C* foo(volatile int);
};

C* Y::foo(volatile int) { return 0; }
