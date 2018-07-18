// { dg-do compile { target c++14 } }
// PR 83406, lambda late returns are not the same as missing returns

class Bar
{
public:
  const int& getter() const;
  int& getter();
};

auto one = [](const Bar& bar) -> decltype(auto)
{
  return bar.getter();
};

auto two = [](const Bar& bar) -> auto
{
  return bar.getter();
};

auto three = [](const Bar& bar)
{
  return bar.getter();
};

template <typename T, typename U> struct X 
{
  static const bool same = false;
};

template <typename T> struct X<T,T>
{
  static const bool same = true;
};

void frob (Bar &x)
{
  static_assert (X<const int &, decltype (one (x))>::same, "not const int &");
  static_assert (X<int, decltype (two (x))>::same, "not int");
  static_assert (X<int, decltype (three (x))>::same, "not int");
}
