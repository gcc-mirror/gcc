// PR c++/84325
// { dg-do compile { target c++17 } }

struct seconds { int i_{0}; constexpr seconds (int) {} };
template <char... _Digits> constexpr seconds operator""_s() {
  return seconds(0);
}
constexpr seconds operator""_s(long double i) {
  return seconds(0);
}
template<class TYPE>
struct Param {
  constexpr static inline seconds time_to_wait{10_s};
};
struct Empty {};
Param<Empty> p;
