// PR c++/53473
// { dg-do compile { target c++11 } }

template<typename T> struct A
{
  static constexpr T foo() noexcept { return 0; }
};

template<> constexpr int A<int>::foo() noexcept { return 0; }
