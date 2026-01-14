// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Dependent variable templates.

using info = decltype(^^void);

template<typename T>
constexpr T magic = 1234;

void foo (int);

template<info R>
void
f ()
{
  int i = template [:R:]<int>;
  constexpr int ci = template [:R:]<int>;
  foo (template [:R:]<int>);
}

template<auto R>
void
f2 ()
{
  int i = [:R:];
  constexpr int ic = [:R:];
  foo ([:R:]);
}

constexpr info V = ^^magic;
constexpr info VT = ^^magic<int>;

void
g ()
{
  f<^^magic>();
  f<V>();
  f2<^^magic<int>>();
  f2<VT>();
}
