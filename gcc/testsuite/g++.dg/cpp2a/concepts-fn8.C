// PR c++/111703
// { dg-do compile { target c++20 } }

template<class T>
constexpr bool always_true() { return true; }

struct P {
  P() = default;

  template<class T>
    requires (always_true<T>()) // { dg-bogus "used before its definition" }
  constexpr P(const T&) { }

  int n, m;
};

void (*f)(P);
P (*h)(P);

template<class T>
constexpr bool g() {
  P x;
  f(x); // { dg-bogus "from here" }
  f(h(x)); // { dg-bogus "from here" }
  return true;
}
