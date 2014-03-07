// Test that we handle merging with deferred noexcept.
// { dg-do compile { target c++11 } }

template <class U>
struct O
{
  template <class T>
  void f() noexcept(noexcept(T()));
};

template<> template<> void O<int>::f<int>() noexcept { }
