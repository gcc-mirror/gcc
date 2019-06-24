// PR c++/84698
// { dg-do compile { target c++11 } }

template<typename A, typename B>
struct X {
  void swap(X& o) noexcept { }

  template<typename... Args>
  friend void swap(X<Args...>& a, X<Args...>& b) noexcept(noexcept(a.swap(b)));
};

template<typename... Args>
inline void swap(X<Args...>& a, X<Args...>& b) noexcept(noexcept(a.swap(b)))
{
}

int
main ()
{
  X<int, int> x;
}
