// PR c++/121325
// { dg-do compile { target c++26 } }

void f(auto... a) requires requires { []<int i = 0> noexcept(noexcept(a...[i])) { }(); } {}
void g(auto... a) requires requires { []<int i = 0> { static_assert(noexcept(a...[i])); }(); } {}

void
h ()
{
  f (0);
  g (0);
}

void foo () {}
void bar () noexcept {}
template<bool B>
void baz () noexcept(B) {}

template<typename... Ts>
void
x (Ts... ts) noexcept (noexcept (ts...[0]()))
{
}

void
y ()
{
  static_assert (!noexcept (x (foo)));
  static_assert (noexcept (x (bar)));
  static_assert (noexcept (x (baz<true>)));
  static_assert (!noexcept (x (baz<false>)));
}
