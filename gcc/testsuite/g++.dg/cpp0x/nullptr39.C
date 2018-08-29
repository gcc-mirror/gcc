// PR c++/81671
// { dg-do compile { target c++11 } }

namespace std { typedef decltype(nullptr) nullptr_t; }

template<class R, class CB> struct Bar
{};
template<class R> struct Bar<R, std::nullptr_t>
{
    template<std::nullptr_t> struct Bind { constexpr static int const cb = 0; };
};
int foo()
{
  return Bar<int, decltype(nullptr)>::Bind<nullptr>::cb;
}
