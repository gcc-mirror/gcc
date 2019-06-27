// PR c++/87250
// { dg-do compile { target c++11 } }
// { dg-options "-Os -fsyntax-only" }

template <typename> struct a {
  constexpr a(int) {}
};
template <typename> struct atomic;
template <> struct atomic<bool> {
  a<bool> b;
  constexpr atomic(bool c) : b(c) {}
};
