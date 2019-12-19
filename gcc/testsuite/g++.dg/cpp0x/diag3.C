// PR c++/59655
// { dg-do compile { target c++11 } }

template<typename T> struct A { static constexpr bool value = false; };

struct B {
  template<typename T>
  B (T t)
  {
    static_assert (A<T>::value, "baz");		// { dg-error "static assertion failed" }
    foo (t);
  }
  template<typename T> void foo (T) {}		// { dg-bogus "used but never defined" }
};

int
main ()
{
  B t([](int) { });
}
