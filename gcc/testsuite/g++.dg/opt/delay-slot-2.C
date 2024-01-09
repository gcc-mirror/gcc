// PR rtl-optimization/113140
// Reduced testcase by Rainer Orth <ro@gcc.gnu.org>

// { dg-options "-O -w" }

int *m();
struct StaticValue {
  long _val;
  void setM(int *) { _val = 0; }
};
struct Value : StaticValue {
  template <typename T> T *as();
};
Value *alloc();
struct Scoped {
  Scoped() {
    Value v;
    ptr = alloc();
    Value *__trans_tmp_1 = v.as<Value>();
    ptr->setM(__trans_tmp_1 ? m() : 0);
  }
  Value *ptr;
};
struct QObjectMethod {
  unsigned long long callInternal() const;
};
unsigned long long QObjectMethod::callInternal() const {
  [] {
    if (Scoped(); 0)
      ;
  }();
}
