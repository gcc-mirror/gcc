// PR c++/122785
// { dg-do compile { target c++26 } }
// { dg-additional-options "-fmodule-header -freflection" }
// { dg-module-cmi {} }

struct S {
  friend S foo();
  S bar(int);
};
inline S s;
template <typename T> decltype(s.bar(T{})) foo(T);
