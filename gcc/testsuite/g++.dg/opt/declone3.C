// PR c++/79176
// { dg-do compile { target c++11 } }
// { dg-require-effective-target lto }
// { dg-options "-flto -Os" }

struct A {};
struct Object {
  virtual bool m_fn1();
  virtual ~Object();
};
struct Item : Object, virtual A {
  ~Item() {
    [] {};
  }
  bool m_fn1();
};
bool Item::m_fn1() {}
