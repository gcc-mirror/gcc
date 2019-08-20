// { dg-additional-options "-fmodules-ts -fmodule-mapper=[srcdir]/map-2.map" }


// Ick!  no cross-host testing for you!
// { dg-additional-files map-2.map }

export module foo; // { dg-error "" }

// { dg-prune-output "not writing module" }
