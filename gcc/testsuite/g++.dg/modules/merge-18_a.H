// PR c++/116803
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

namespace ns {
  template <typename T> void foo();
  template <typename T> extern const int bar;
}
