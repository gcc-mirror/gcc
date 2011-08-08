// PR c++/49808

template <class X, X g>
struct A
{
  A() { float r = g(0); }
};

struct f_t
{
  float operator() (float) const { return 1; }
};

f_t f;

A<f_t&, f> x;
