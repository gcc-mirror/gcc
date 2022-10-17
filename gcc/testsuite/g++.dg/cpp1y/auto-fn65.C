// PR c++/106893
// { dg-do compile { target c++14 } }

template <typename T>
struct CoordTraits
{
  static auto GetX(T const &p) { return 1; }
};
typedef CoordTraits<int> Traits;
static constexpr auto GetX = Traits::GetX;
