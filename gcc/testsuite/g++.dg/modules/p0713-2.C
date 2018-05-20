// { dg-additional-options -fmodules-ts }

int j;
module; // { dg-error "does not follow" }
// { dg-error "expected module-name" "" { target *-*-* } .-1 }
