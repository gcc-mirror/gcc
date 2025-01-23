// PR c++/118245
// { dg-do compile { target c++20 } }

template<auto> struct Cask {};
struct T1 : Cask<[]{}> {
  Cask<[]{}> c{};
};
