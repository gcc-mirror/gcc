// PR c++/92193
// { dg-do compile { target c++11 } }

template<typename T>
  struct has_foo
  { static constexpr bool value = false; };

template<typename T>
#ifndef NO_CONSTEXPR
  constexpr
#endif
  bool
  foo(T t) noexcept(noexcept(t.foo()))
  { return t.foo(); }

template<typename T>
  void
  maybe_foo(T t)
  {
    static_assert( has_foo<T>::value, "has foo" ); // { dg-error "has foo" }
    foo(t);
  }

struct X { };

int main()
{
  X x;
  maybe_foo(x);
}
