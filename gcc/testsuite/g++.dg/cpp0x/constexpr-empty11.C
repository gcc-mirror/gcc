// PR c++/67364
// { dg-do compile { target c++11 } }

template <typename Xn>
struct element : Xn {
  constexpr element() : Xn() { }
};

template <typename Xn>
struct closure {
  element<Xn> member;
  constexpr closure() { }
};

struct empty { struct {} s; };
constexpr closure<empty> tup{};
constexpr empty first = tup.member;
