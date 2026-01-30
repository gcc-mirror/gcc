// PR c++/123790
// { dg-do compile { target c++11 } }

int
main ()
{
  using nullptr_t = decltype (nullptr);
  constexpr nullptr_t zero = nullptr;
  constexpr nullptr_t other_zero = zero;
}
