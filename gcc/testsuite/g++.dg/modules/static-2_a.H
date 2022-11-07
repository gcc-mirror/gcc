// PR c++/104433
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

inline int* f() {
  static int y;
  return &y;
}
