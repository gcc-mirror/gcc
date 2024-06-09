// PR c++/113708
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

inline int f() { return 42; }
inline int a = f();
