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

template<C1... Ts> struct S1 { }; // OK
S1<int, int, char> s1; // { dg-error "constraint failure|invalid type" }
template<C1 Ts> struct S2 { }; // { dg-error "variadic constraint"  }

template<C2... Bs> struct S3 { }; // OK
S3<true, true, false> s3; // { dg-error "constraint failure|invalid type" }
template<C2 Bs> struct S4 { }; // { dg-error "variadic constraint" }

int main() { }
