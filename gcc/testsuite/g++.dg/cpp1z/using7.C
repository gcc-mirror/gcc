// PR c++/91673 - ICE with noexcept in alias-declaration.
// { dg-do compile { target c++17 } }

template<typename T, bool B>
using U1 = T() noexcept(B);

template<bool B>
struct S {
  int I;
  static constexpr bool b = true;

  template<typename T>
  using U2 = T() noexcept(B);

  template<typename T>
  using U8 = T() noexcept(b);

  template<typename T>
  using U10 = T(int p) noexcept(noexcept(p));

  template<typename T, bool B2>
  using U11 = T() noexcept(B2);

  using U3 = void() noexcept(B);
  using U9 = void() noexcept(b);
  using U4 = void() noexcept(noexcept (I));
  using U5 = void(int p) noexcept(noexcept(p));

  typedef void(*T1)() noexcept(B);
  typedef void(*T2)(int p) noexcept(noexcept(p));
};

S<true> s;
