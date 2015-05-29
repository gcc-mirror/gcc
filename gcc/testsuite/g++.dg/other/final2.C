// PR c++/53186
// { dg-do compile { target c++11 } }
// { dg-options "-fdump-tree-original"  }

struct F1
{
  virtual void operator()() final;
  virtual operator int() final;
  virtual int operator++() final;
};

struct F2 final
{
  virtual void operator()();
  virtual operator int();
  virtual int operator++();
};

void fooF1(F1& a) { a(); int m = a; ++a; }
void fooF2(F2& a) { a(); int m = a; ++a; }

// { dg-final { scan-tree-dump-times "F1::operator\\(\\)" 1 "original" } }
// { dg-final { scan-tree-dump-times "F1::operator int" 1 "original" } }
// { dg-final { scan-tree-dump-times "F1::operator\\+\\+" 1 "original" } }
// { dg-final { scan-tree-dump-times "F2::operator\\(\\)" 1 "original" } }
// { dg-final { scan-tree-dump-times "F2::operator int" 1 "original" } }
// { dg-final { scan-tree-dump-times "F2::operator\\+\\+" 1 "original" } }
