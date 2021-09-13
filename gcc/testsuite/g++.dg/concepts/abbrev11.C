// PR c++/99806
// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts" }

template <typename T> concept C = requires (T a) { a.f(0); };
struct S {
  void f(auto) noexcept(B);
  static constexpr bool B = true;
};
static_assert(C<S>, "");
