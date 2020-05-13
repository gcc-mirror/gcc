// PR c++/90099
// { dg-do compile { target c++20 } }

struct Unit {
  int value;
  // auto operator<=>(const Unit&) = default;
};

template<Unit U, typename... Ts>
struct X {};

template<Unit U, typename T, typename... Rest>
struct X<U, T, Rest...> {};
