// Allow static_assert in constexpr constructors, too.
// { dg-options -std=c++11 }

template<typename T>
struct A
{
  int i;

  constexpr A(int i) : i(i)
  {
    static_assert(sizeof(T) == 1, "");
  }
};
