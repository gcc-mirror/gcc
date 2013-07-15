// PR c++/57874
// { dg-do compile { target c++11 } }

namespace NX
{
  struct X {};
  void foo(X) {}
}

namespace NY
{
  struct Y {};
}

template<class T>
auto ADLfoo(T&&) -> decltype((foo(T{}), short()));

char ADLfoo(...);

static_assert(sizeof(ADLfoo(NY::Y{})) == 1, "");
static_assert(sizeof(ADLfoo(NX::X{})) == 2, "");
