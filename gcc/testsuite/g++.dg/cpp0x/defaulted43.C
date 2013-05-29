// PR c++/53903
// { dg-do compile { target c++11 } }

struct T
{
  T() noexcept(false) { }
  ~T() noexcept(false) { }
};

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

struct B
{
  B() noexcept(false);
  ~B() noexcept(false);

  U u;
};

B::B() noexcept(false) = default;
B::~B() noexcept(false) = default;

struct V
{
  V() noexcept(false) { }
  ~V() noexcept(false) { }
};

struct C
{
  C() noexcept = default;     // { dg-error "defaulted" }
  ~C() noexcept = default;    // { dg-error "defaulted" }

  V v;
};
