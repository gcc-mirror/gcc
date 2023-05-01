// PR c++/109666
// { dg-do compile { target c++11 } }

struct Point {
  int value_;
};
template <int n> struct StaticVector {
  static StaticVector create() {
    StaticVector output;
    return output;
  }
  Point _M_elems[n]{};

};
void f() { StaticVector<3>::create(); }
