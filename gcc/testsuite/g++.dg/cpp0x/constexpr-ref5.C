// PR c++/50473
// { dg-do compile { target c++11 } }

constexpr int f() { return 1; }

template<class T>
struct test
{
  static constexpr const auto& value = f();
  int a[value];
};

test<int> t;
