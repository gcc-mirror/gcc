// { dg-do assemble  }
// Origin: Theodore Papadopoulo <Theodore.Papadopoulo@sophia.inria.fr>

struct H {
  template <class T> void k() const { }
  typedef void (H::*pmf)() const;

  pmf f() const { return &H::k<int>; }
};
