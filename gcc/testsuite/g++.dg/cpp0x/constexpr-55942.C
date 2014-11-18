// PR c++/55942
// { dg-do compile { target c++11 } }

struct A
{
  constexpr explicit A(bool b) : o{flip(b)} { }

  constexpr bool flip(bool b) { return !b; }

  bool o;
};
