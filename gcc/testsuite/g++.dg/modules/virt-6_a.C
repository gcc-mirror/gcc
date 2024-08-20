// PR c++/115007
// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi M:a }

module;
struct S {
  virtual ~S() = default;
  virtual void f() = 0;
};
module M:a;
extern S* p;
template <typename T> void format(T) { p->~S(); }
template void format(int);
