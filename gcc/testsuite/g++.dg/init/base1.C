// PR c++/68983

class SvxOptionsGrid {
  int nFldDrawX;
  bool bEqualGrid;
public:
  ~SvxOptionsGrid();
};
class A : SvxOptionsGrid {
public:
  A(SvxOptionsGrid p1) : SvxOptionsGrid(p1) {}
};
SvxOptionsGrid a;
void fn1() { A b(a); }
