// PR c++/68348
// { dg-do compile { target c++11 } }

struct C {
  constexpr C() : w(), x(), y() {}
  constexpr double fn() const noexcept;
  double w;
  double x;
  double y;
};

constexpr double C::fn() const noexcept { return w; }
C foo()
{
  C c;
  c.fn ();
  return c;
}
