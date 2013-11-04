// PR c++/53903
// { dg-do compile { target c++11 } }

struct T
{
  T() noexcept(false) { }
  ~T() noexcept(false) { }
};

T t;

struct A
{
  A() noexcept;
  ~A() noexcept;

  T t;
};

A::A() noexcept = default;   // { dg-error "defaulted" }
A::~A() noexcept = default;  // { dg-error "defaulted" }

struct U
{
  U() noexcept(false) { }
  ~U() noexcept(false) { }
};

U u;

struct B
{
  B() noexcept(false);
  ~B() noexcept(false);

  U u;
};

B::B() noexcept(false) = default;
B::~B() noexcept(false) = default;

B b;

struct V
{
  V() noexcept(false) { }
  ~V() noexcept(false) { }
};

V v;

struct C
{
  C() noexcept = default;	// { dg-message "exception-specification" }
  ~C() noexcept = default;	// { dg-message "exception-specification" }

  V v;
};

C c;				// { dg-error "deleted" }
