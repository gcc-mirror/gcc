// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^void);

template<typename T>
constexpr int
fn (T t)
{
  return t;
}

template<typename T>
constexpr T
foo ()
{
  constexpr auto r = ^^fn<T>;
  int n = [: r :](42);
  return n;
}

void
g ()
{
  constexpr int r = foo<int>();
  static_assert (r == 42);
}

template<typename T>
consteval info
h ()
{
  return ^^T;
}

constexpr info i = h<int>();
