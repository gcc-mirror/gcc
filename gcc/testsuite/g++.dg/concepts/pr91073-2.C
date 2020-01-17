// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename P, typename Arghhh = void>
concept one_or_two = true;

template<typename P>
concept one = one_or_two<P>;

template<typename T>
constexpr void
foo()
{
  if (one<T>) // OK
  { }

  if (one_or_two<T>) // { dg-bogus "before" }
  { }
}
