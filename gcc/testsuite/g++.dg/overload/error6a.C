// Verify we suggest -fdiagnostics-all-candidates when there are
// omitted candidates.
#include "error6.C"

// { dg-error "no match" "" { target *-*-* } 9 }
// { dg-message "use '-fdiagnostics-all-candidates'" "" { target *-*-* } 9 }
