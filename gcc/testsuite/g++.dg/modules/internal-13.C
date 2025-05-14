// PR c++/119996
// { dg-additional-options "-fmodules" }
// { dg-module-cmi !M }
// Similar to internal-11.C, but for potentially-constant variables.

export module M;

static int tu_local = 5;
static int& foo() { return tu_local; }

// For implementation reasons, we adjust [basic.link] p14.2 to restrict ignored
// exposures to non-inline variables, since for inline variables without
// dynamic initialisation we need to emit their initialiser for importer use.

int& a = tu_local;  // OK
inline int& b = tu_local;  // { dg-error "initialized to a TU-local value" }
inline auto& bf = foo;  // { dg-error "initialized to a TU-local value" }

// But dynamic initialisers are fine, importers will just treat them as external.
inline int& c = foo();  // OK

// For consistency, we follow the same rules with templates, noting that
// we still need to emit definitions with dynamic initializers so we error.
template <typename T> int& d = tu_local;  // OK
template <typename T> inline int& e = tu_local;  // { dg-error "exposes TU-local entity" }
template <typename T> inline int& f = foo();  // { dg-error "exposes TU-local entity" }
template <typename T> inline auto& ff = foo;  // { dg-error "exposes TU-local entity" }

// Note that non-references are OK, because an integer or enumeration
// value is never TU-local: we fold these expressions early
// (as we should, by [basic.link] p14.4).
static const int const_val = 123;
inline const int potentially_constant = const_val;  // OK
