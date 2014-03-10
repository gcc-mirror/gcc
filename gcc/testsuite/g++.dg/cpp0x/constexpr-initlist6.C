// PR c++/55419
// { dg-do compile { target c++11 } }

struct P
{
  P () = default;
  explicit constexpr P (int x) : p (x) {}
  int p;
};

struct Q
{
  constexpr Q () : q (0x7f) {}
  int q;
};

struct R
{
  Q q;
  P p;
};

void
foo (R *x)
{
  *x = {};
}
