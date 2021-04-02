// { dg-additional-options "-fmodules-ts" }

#define EXPORT export  // { dg-error "only occur after a module" }
EXPORT module bob; // { dg-error "does not name a type" }
// { dg-message "not recognized as" "" { target *-*-* } .-1 }

