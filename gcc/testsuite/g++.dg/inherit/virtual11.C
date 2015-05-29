// PR c++/59031
// { dg-do compile }
// { dg-options "-fdump-tree-gimple " }
class B {
 public:
  virtual int add (int a, int b) {return a+ b;}
};

class D : public B {
};

int foo (int a, int b) {
  D d;
  return d.add(a, b);
}
// { dg-final { scan-tree-dump-not "OBJ_TYPE_REF" "gimple" } }
