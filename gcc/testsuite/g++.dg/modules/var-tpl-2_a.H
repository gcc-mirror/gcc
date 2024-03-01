// PR c++/114170
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

inline int f() { return 42; }

template<class>
inline int v = f();

inline int g() { return v<int>; }
