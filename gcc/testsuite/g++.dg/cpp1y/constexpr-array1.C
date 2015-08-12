// PR c++/67104
// { dg-do compile { target c++14 } }

template <typename T, int N> struct array
{
  constexpr T &operator[](int index) { return data[index]; }
  constexpr T operator[](int index) const { return data[index]; }
  T data[N];
};

constexpr array<long unsigned, 1001>
make_bottle_count ()
{
  array<long unsigned, 1001> a{};
  a[65] = 1;
  return a;
}

constexpr auto bottle_count = make_bottle_count ();
static_assert (bottle_count[65], "");
