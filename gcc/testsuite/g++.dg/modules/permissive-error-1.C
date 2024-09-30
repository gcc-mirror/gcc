// { dg-additional-options "-fmodules-ts -fpermissive" }
// { dg-module-cmi !M }

export module M;  // { dg-error "not writing module" }

template <typename T> void f() {
  const int n = 42;
  ++n;  // { dg-warning "read-only" }
	// { dg-message "appeared here" "" { target *-*-* } .-1 }
}
