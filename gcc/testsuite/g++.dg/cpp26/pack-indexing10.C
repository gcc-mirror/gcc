// P2662R3 - Pack Indexing
// PR c++/113798
// { dg-do compile { target c++26 } }

template<auto... Vs>
constexpr auto f() {
  return []<int N>() { return Vs...[N]; }.template operator()<1>();
}
static_assert(f<1, 2, 3>() == 2);

template<int N>
constexpr auto g() {
  return []<auto... Vs>() { return Vs...[N]; }.template operator()<1, 2, 3>();
}
static_assert(g<1>() == 2);
