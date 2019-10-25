// { dg-do compile { target c++2a } }

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

template<typename... Ts>
concept Same = are_same<Ts...>::value;

template<typename T>
concept Int = __is_same_as(T, int);

// NOTE: The use of Same is misleading; it constraints each T, not
// the pack of Ts.
template<Same... Ts> struct S1 { }; // requires (C<Ts> && ...)
S1<int, int, int> s1;
S1<int, int, bool> s2;

// // NOTE: The use of Same is misleading; it constraints each deduced
// // type, not the pack of Ts.
template<Same auto... Xs> struct S2 { }; // requires (C<X> && ...) with each X deduced
S2<true, true, true> s3;
S2<true, true, 'a'> s4;  // OK

template<Int... Ts> struct S3 { }; // requires (C<Ts> && ...)
S3<int, int, char> x0; // { dg-error "template constraint failure" }

template<Int auto... Xs> struct S4 { }; // requires (C<X> && ...) with each X deduced
S4<0, 1, 2, 'a'> x1; // { dg-error "placeholder constraints not satisfied" }

