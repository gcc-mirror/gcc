// PR c++/47461

class C {
 public:
  template<typename T> bool f(T* m) __attribute__((warn_unused_result));
};
template<typename T> inline bool C::f(T* m) { return true; }
void f(C* pc) { int i; pc->f(&i); }  // { dg-warning "ignoring return value" }
