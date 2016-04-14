// PR c++/70648
// { dg-do compile { target c++11 } }

struct C
{
  template <class... U>
  constexpr C (...) : c { static_cast<U &&>(0)... } {}
  int c[1];
};

static constexpr int b = C{}.c[0];
