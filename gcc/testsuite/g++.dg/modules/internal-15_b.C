// { dg-additional-options "-fmodules -pedantic-errors" }

module A;

void other() {
  adl(N::A{});  // OK, lookup occurs from here
  h(0);  // OK, doesn't consider N::inner::adl

  h(N::A{});  // { dg-message "required from here" }
  // { dg-error "TU-local" "" { target *-*-* } 0 }

  h(G::B{});  // OK, G::adl is not attached to A
}
