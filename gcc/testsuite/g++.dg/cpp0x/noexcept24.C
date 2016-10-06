// PR c++/53025
// { dg-do compile { target c++11 } }

template<typename T>
struct A {
  A() noexcept {}
  A(const A&) noexcept(false) {}
};

template<typename T>
void a(A<T>) noexcept {}

template<typename T>
void f()
{
#if __cplusplus <= 201402L
  const bool val = false;
#else
  const bool val = true;
#endif
  static_assert(val == noexcept(a(A<T>{})), "");
}

void g()
{
  f<int>();
}
