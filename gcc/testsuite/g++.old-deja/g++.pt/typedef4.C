// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct B1 {
  typedef int X;
};

struct B2 {
  typedef int X;
};

template <class T>
struct D : public B1, public B2 {
  typedef int X;
};

template struct D<int>;
