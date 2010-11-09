// Allow static_assert in constexpr constructors, too.
// { dg-options -std=c++0x }

template<typename T>
struct A
{
  int i;

  constexpr A(int i) : i(i)
  {
    static_assert(sizeof(T) == 1, "");
  }
};
