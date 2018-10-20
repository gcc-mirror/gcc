// PR c++/83918
// { dg-do compile { target c++17 } }

constexpr unsigned
foo (unsigned x, unsigned y)
{
  return x > y ? x : y;
}

template <typename, typename> struct A;
template <auto ...> struct B;
template <auto S, auto ... T, auto U, auto ... V>
struct A <B <S, T...>, B <U, V...>>
{
  enum : unsigned
  {
    u = foo (sizeof (S), sizeof (U)),
    v = A <B <T...>, B <V...>>::w,
    w = foo (u, v)
  };
};

template <>
struct A <B <>, B <>>
{
  enum : unsigned { w = 0 };
};

constexpr static const auto v { A <B <1,2,3,4,5,6,7,8,9>,
				   B <9,8,7,6,5,4,3,2,1>>::w };
static_assert (v == sizeof (int));
