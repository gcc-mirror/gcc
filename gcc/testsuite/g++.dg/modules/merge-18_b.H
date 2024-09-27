// PR c++/116803
// { dg-additional-options "-fmodule-header -fdump-lang-module" }
// { dg-module-cmi {} }

import "merge-18_a.H";

namespace ns {
  template <typename T> void foo() {}
  template <typename T> const int bar = 123;
}

// { dg-final { scan-lang-dump {Writing definition '::ns::template foo'} module } }
// { dg-final { scan-lang-dump {Writing definition '::ns::template bar'} module } }
