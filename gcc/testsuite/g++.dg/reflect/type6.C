// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^int);

template<typename T> concept C = requires {
  typename [:T::r1:];
  typename [:T::r2:]<int>;
};

template<typename T>
struct Z { };

struct S {
  static constexpr info r1 = ^^int;
  static constexpr info r2 = ^^Z;
};

void
g (S s)
{
  typename [: S::r1 :] i = 42;
  typename [: S::r2 :]<int> z;
  C auto a = s;
}
