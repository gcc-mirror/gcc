// PR c++/94716
// { dg-do compile { target c++14 } }
// { dg-additional-options "-fdelete-null-pointer-checks" }

template <int> char v = 0;
static_assert (&v<2> == &v<2>, "");
static_assert (&v<0> != &v<1>, "");
constexpr bool a = &v<2> == &v<2>;
constexpr bool b = &v<0> != &v<1>;
