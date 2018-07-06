// PR c++/85545
// { dg-do compile { target c++11 } }

struct A
{
  constexpr int foo() const noexcept { return 1; }
};

constexpr auto p = static_cast<int (A::*)() const>(&A::foo);
constexpr int i = (A().*p)();

#define SA(X) static_assert((X),#X)
SA(i == 1);
