// PR c++/53025
// { dg-do compile { target c++11 } }

struct A {
  A() noexcept {}
  A(const A&) noexcept(false) {}
};

void a(A) noexcept {}

void f()
{
#if __cplusplus <= 201402L
  const bool val = false;
#else
  const bool val = true;
#endif
  static_assert(noexcept(a(A{})) == val, "");
}
