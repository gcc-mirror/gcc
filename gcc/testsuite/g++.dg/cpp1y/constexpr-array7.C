// PR c++/71504
// { dg-do compile { target c++14 } }

template <typename A>
constexpr auto
sum (A const &a)
{
  int tot = 0;
  for (auto &row : a)
    for (auto elem : row)
      tot += elem;
  return tot;
}

constexpr int const a22[2][2] = {{1,2},{3,4}};
static_assert (sum(a22) == 10, "badsum");
