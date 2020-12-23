// { dg-additional-options "-fmodules-ts -fmodule-mapper=[srcdir]/map-2.map" }


// Ick!  no cross-host testing for you!
// { dg-additional-files map-2.map }

export module foo;
// { dg-error "Interface: no such module" "" { target *-*-* } .-1 }
// { dg-error "failed reading mapper" "" { target *-*-* } 0 }

// { dg-prune-output "not writing module" }
