// PR c++/52816
// { dg-do compile { target c++11 } }

class c {
  int f;
  public:
  template <typename A>
  decltype(f) m(A) const;
};

decltype(c{}.m(0)) i;
