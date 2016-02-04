// { dg-do compile }
// { dg-options "-O3 -fdump-tree-optimized -std=c++14" }

#include <array>
#include <utility>


template<class Key, class T, size_t N> struct static_map
{
  using key_type = Key;
  using mapped_type = T;
  using value_type = std::pair<const key_type, mapped_type>;
private:
  using _value_type = std::pair<size_t, value_type>;
  _value_type _values[N];
  static constexpr _value_type _new_value_type(const std::pair<Key, T> &v)
  {
    return std::make_pair(0, std::make_pair(v.first, v.second));
  }
public:
  template<class... U> constexpr static_map(U &&...il) : _values{ _new_value_type(il)... } { }
  constexpr mapped_type &operator[](const key_type &k) { return at(k); }
  constexpr const mapped_type &operator[](const key_type &k) const { return at(k); }
  constexpr mapped_type &at(const key_type &k)
  {
    for (size_t n = 0; n < N; n++)
      if (_values[n].second.first == k)
        return _values[n].second.second;
    throw std::out_of_range("Key not found");
  }
  constexpr const mapped_type &at(const key_type &k) const
  {
    for (size_t n = 0; n < N; n++)
      if (_values[n].second.first == k)
        return _values[n].second.second;
    throw std::out_of_range("Key not found");
  }
};
namespace detail
{
  template<class Key, class T, size_t N, size_t... I> constexpr static_map<Key, T, N> static_map_from_array(const std::pair<Key, T>(&il)[N], std::index_sequence<I...>)
  {
    return static_map<Key, T, N>(il[I]...);
  }
}
template<class Key, class T, size_t N> constexpr static_map<Key, T, N> make_static_map(const std::pair<Key, T> (&il)[N])
{
  return detail::static_map_from_array<Key, T, N>(il, std::make_index_sequence<N>());
}

/* Two phase construction, required because heterogeneous braced init
in C++ 14 has a big limitation: template<class... Args> auto make(Args &&...)
will accept make({ 5, "apple" }) as make(int, const char *) but
make({ 5, "apple" }, { 8, "pear" }) will fail to deduce Args as a
heterogeneous initializer_list is not permitted. This forces something
like make(make_pair{ 5, "apple" }, make_pair{ 8, "pear" }, ...) which
is less succinct than using a constexpr C array for the nested braced init.
*/
constexpr std::pair<const int, const char *> map_data[] = {
  { 5, "apple" },
  { 8, "pear" },
  { 0, "banana" }
};

template<size_t N> constexpr int cstrcmp(const char *a, const char *b)
{
  for (size_t n = 0; n < N; n++)
  {
    if (a[n] < b[n]) return -1;
    if (a[n] > b[n]) return 1;
  }
  return 0;
}

int main(void)
{
  constexpr auto cmap = make_static_map(map_data);
  // No abort() appears in assembler, so this was executed constexpr
  if(!cmap[8]) abort();
  // This however does cause code implementing a lookup to be generated,
  // so this was NOT executed constexpr
  //const char *foo=cmap[5];
  return 0;
}

// { dg-final { scan-tree-dump-not "cmap" "optimized" { target x86_64-*-* i?86-*-* } } }
