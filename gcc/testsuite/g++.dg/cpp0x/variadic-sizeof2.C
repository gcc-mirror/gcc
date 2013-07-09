// PR c++/57471
// { dg-require-effective-target c++11 }

struct A
{
  static constexpr bool value = true;
};

template<typename... Types>
struct B
{
  static_assert(A::value, "");
  static_assert(sizeof...(Types) == 0, "");
};
