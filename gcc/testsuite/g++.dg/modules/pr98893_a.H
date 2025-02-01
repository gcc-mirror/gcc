// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

struct S {
  ~S() {}
};
inline void foo() {
  static S a[1];
}
