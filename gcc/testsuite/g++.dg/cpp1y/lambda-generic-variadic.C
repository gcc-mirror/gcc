// Basic generic lambda test
// { dg-do run { target c++14 } }

template <typename T, typename U> struct pair {};
template <typename... T> struct tuple {};

int main()
{
  auto a = [] (auto, pair<auto,auto> v) { return sizeof (v); };
  auto b = [] (auto, pair<pair<auto,auto>,auto>... v) { return sizeof... (v); };

  a(1, pair<int, float>());
  b(2, pair<pair<short,char>, double>(), pair<pair<float,long>, int>());
}
