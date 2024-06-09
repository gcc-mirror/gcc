// PR c++/113638
// { dg-do compile { target c++14 } }

template<int ...Is>
constexpr int my_array[]{Is...};
constexpr auto t1 = my_array<2>;
static_assert(sizeof(my_array<1>) == sizeof(int) * 1, "");
