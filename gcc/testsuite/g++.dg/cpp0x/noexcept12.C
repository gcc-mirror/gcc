// Test that we handle merging with deferred noexcept.
// { dg-options -std=c++0x }

template <class U>
struct O
{
  template <class T>
  void f() noexcept(noexcept(T()));
};

template<> template<> void O<int>::f<int>() noexcept { }
