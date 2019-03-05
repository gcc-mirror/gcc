// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename... Ts> struct are_same;

template<>
  struct are_same<> {
    static constexpr bool value = true;
  };

template<typename T>
  struct are_same<T> {
    static constexpr bool value = true;
  };

template<typename T, typename U, typename... Ts>
  struct are_same<T, U, Ts...> {
    static constexpr bool value =
      __is_same_as(T, U) && are_same<U, Ts...>::value;
  };

constexpr bool all_of() { return true; }
constexpr bool all_of(bool b) { return b; }

template<typename... Ts>
  constexpr bool all_of(bool a, bool b, Ts... args) {
    return (a && b) && all_of(b, args...);
  }

template<typename... Ts>
  concept bool C1 = are_same<Ts...>::value;

template<bool... Bs>
  concept bool C2 = all_of(Bs...);

template<C1... Ts> struct S1 { };
template<C1...> struct S2 { };
template<C2... Bs> struct S4 { };
template<C2...> struct S5 { };

S1<int, int, int> s1;
S4<true, true, true> s4;
