// PR c++/109876
// { dg-do compile { target c++11 } }

using size_t = decltype(sizeof 0);

namespace std {
template <class> struct initializer_list {
  const int *_M_array;
  size_t _M_len;
  constexpr size_t size() const { return _M_len; }
};
} // namespace std

constexpr std::initializer_list<int> gnum{2};

template <int> struct Array {};
template <int> void g()
{
  static constexpr std::initializer_list<int> num{2};
  static_assert(num.size(), "");
  Array<num.size()> ctx;

  constexpr Array<1> num1{};
}

template <int N>
struct Foo
{
  static constexpr std::initializer_list<int> num = { 1, 2 };
  static_assert(num.size(), "");
  Array<num.size()> ctx;
};

void
f (Foo<5>)
{
  g<0>();
}
