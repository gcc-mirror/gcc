// { dg-additional-options "-fmodules-ts -Wno-template-body" }
// { dg-module-cmi !M }

export module M;  // { dg-error "not writing module" }

template <typename T> void f() {
  const int n = 42;
  ++n;  // { dg-message "appeared here" }
}

// { dg-message "enable \[^\n\r\]* for more details" "" { target *-*-* } 0 }
