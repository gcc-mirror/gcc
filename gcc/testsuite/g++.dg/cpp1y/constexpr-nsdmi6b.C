// PR c++/94034
// { dg-do compile { target c++14 } }

struct A {
  A() = default; A(const A&);
  A *ap = this;
};

constexpr A foo()
{
  return {};
}

constexpr A bar()
{
  return foo();
}

void
baz()
{
  constexpr A a = foo(); // { dg-error ".A..& a... is not a constant expression" }
  constexpr A b = bar(); // { dg-error ".A..& b... is not a constant expression" }
}

constexpr A a = foo();
constexpr A b = bar();
