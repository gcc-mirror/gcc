// PR c++/106849
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi !lib }

export module lib;

namespace outer {
  template<typename T> void any_of(T) { }  // { dg-note "declared here" }
}

export using outer::any_of;  // { dg-error "does not have external linkage" }
