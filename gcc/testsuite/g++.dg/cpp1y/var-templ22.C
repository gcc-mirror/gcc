// PR c++/63889
// { dg-do compile { target c++14 } }

template<class T>
struct A
{
  template<class>
  static constexpr bool is_ok = true;

  template<bool v = is_ok<T>>
  A(T) { }
};

A<int> p(42);
