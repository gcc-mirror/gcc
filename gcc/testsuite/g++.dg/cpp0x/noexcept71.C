// PR c++/99980
// { dg-do compile { target c++11 } }

#define SA(X) static_assert(X, #X)

struct S {
  template<typename T>
  void f(T) noexcept(B);

  struct N {
    template<typename T>
    void f2(T) noexcept(B);
  };

  static constexpr bool B = true;
};

S s;
SA(noexcept(s.f(10)));
S::N n;
SA(noexcept(n.f2(10)));

struct Bad {
  template<typename T>
  using U = void() noexcept(B); // { dg-error "not declared" }

  template<typename T>
  friend void friendo() noexcept(B); // { dg-error "not declared" }

  static constexpr bool B = true;
};
