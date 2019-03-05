// PR c++/87125
// { dg-do compile { target c++11 } }

template <typename T>
struct S {
  template <typename U>
  constexpr S (U) noexcept (T ()) {}
};
struct V : S<int> { using S::S; };

bool
foo ()
{
  return noexcept (V (0));
}
