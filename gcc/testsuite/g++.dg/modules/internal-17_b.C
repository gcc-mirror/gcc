// PR c++/121576
// { dg-additional-options "-fmodules -Wno-error=expose-global-module-tu-local -Wtemplate-names-tu-local -Wno-global-module" }
// { dg-module-cmi !X }

module;

namespace {
  struct InternalX {};  // { dg-message "internal" }
  // Only used by '::y', so should be discarded and not complain
  struct InternalY {};  // { dg-bogus "" }
}

static inline int x() { // { dg-error "TU-local" }
		        // { dg-message "exposed elsewhere" "" { target *-*-* } .-1 }
  InternalX x;
  return 1;
}

static inline int y() {  // { dg-bogus "" }
  InternalY y;
  return 2;
}

namespace {
  struct S {};
  template <typename> void tmpl();

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wexpose-global-module-tu-local"
  struct S_ignored {};
  template <typename> void tmpl_ignored();
#pragma GCC diagnostic pop
}

export module X;
import M;

void test_usage() {
  a();
  b();
  c<int>();  // { dg-message "required from here" }
  d<int>();  // { dg-bogus "" }
  e();
  f<int>();  // { dg-message "required from here" }
  g();
  h<int>();  // { dg-bogus "" }

  // { dg-warning "instantiation exposes TU-local entity" "" { target *-*-* } 0 }
}

inline void expose() {  // { dg-warning "exposes TU-local" }
  int result = x();
}

// Internal linkage types always hard error
inline void expose_struct() {  // { dg-error "exposes TU-local" }
  S s;
}
inline void still_expose_struct() {  // { dg-error "exposes TU-local" }
  S_ignored s;
}

// Template instantiations occuring in module purview are not ignored,
// as it's too hard to tell if the instantiation will accidentally rely
// on something in the purview or not.
inline void expose_tmpl() {  // { dg-error "exposes TU-local" }
  tmpl<int>();
}
inline void still_expose_tmpl() {  // { dg-error "exposes TU-local" }
  tmpl_ignored<int>();
}
