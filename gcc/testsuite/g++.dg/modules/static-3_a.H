// { dg-additional-options -fmodules }
// { dg-module-do link }

inline int i;

struct A {
  A() { ++i; }
};

inline A& afn() {
  static A a;
  return a;
}
