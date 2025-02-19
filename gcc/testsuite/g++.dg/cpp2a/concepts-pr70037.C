// PR c++/70037
// { dg-do compile { target c++20 } }

namespace std {
  template<typename>
  struct F {
    static constexpr bool value = false;
  };

  template<typename T>
  struct tuple {
    constexpr tuple() requires (F<T>::value)  {}
    explicit constexpr tuple() requires (F<T>::value) && (true) {}
  };
}

using std::tuple;
template struct std::tuple<tuple<int>>;
