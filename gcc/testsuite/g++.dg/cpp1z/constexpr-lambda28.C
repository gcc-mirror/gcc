// PR c++/107280
// { dg-do compile { target c++17 } }

struct string {
  char str[8] = "";
};
template <int, int> constexpr void
test ()
{
  string str{};
  auto append = [&](const char *s) { *str.str = *s; };
  append("");
}

static_assert ((test<true, true>(), true), "");
