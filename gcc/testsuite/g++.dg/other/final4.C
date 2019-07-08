// PR c++/67184
// { dg-do compile { target c++11 } }
// { dg-options "-fdump-tree-original"  }

struct B
{
  virtual void operator()();
  virtual operator int();
  virtual int operator++();
};

struct D final : B { };

void foo(D& d) { d(); int t = d; ++d; }

// { dg-final { scan-tree-dump-times "OBJ_TYPE_REF" 0 "original" } }
