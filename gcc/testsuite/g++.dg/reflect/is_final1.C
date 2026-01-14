// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_final.

#include <meta>

struct A {
  virtual void foo();

  virtual void bar() final;

  virtual ~A() = default;
};

struct B final : A {
  void foo() override;

  ~B() final = default;
};

struct C : A {
  void foo() final;
};

struct D final : A {
  static_assert (is_final (^^D));
};

static_assert (!std::meta::is_final (^^A));
static_assert (std::meta::is_final (^^B));
static_assert (!std::meta::is_final (^^C));

static_assert (!std::meta::is_final (^^A::foo));
static_assert (!std::meta::is_final (^^B::foo));
static_assert (std::meta::is_final (^^C::foo));

static_assert (std::meta::is_final (^^A::bar));
static_assert (std::meta::is_final (^^B::bar));
static_assert (std::meta::is_final (^^C::bar));

static_assert (!std::meta::is_final (^^A::~A));
static_assert (std::meta::is_final (^^B::~B));
static_assert (!std::meta::is_final (^^C::~C));

A a;
B b;
C c;

static_assert (!std::meta::is_final (^^::));
static_assert (!std::meta::is_final (^^a));
static_assert (!std::meta::is_final (^^b));
static_assert (!std::meta::is_final (^^c));

static_assert (!std::meta::is_final (std::meta::type_of (^^a)));
static_assert (std::meta::is_final (std::meta::type_of (^^b)));
static_assert (!std::meta::is_final (std::meta::type_of (^^c)));
