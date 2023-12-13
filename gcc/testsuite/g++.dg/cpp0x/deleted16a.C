// Verify we suggest -fdiagnostics-all-candidates when diagnosing
// overload resolution selecting a deleted function.
// { dg-do compile { target c++11 } }
#include "deleted16.C"

// { dg-error "deleted" "" { target *-*-* } 21 }
// { dg-error "deleted" "" { target *-*-* } 22 }
// { dg-error "deleted" "" { target *-*-* } 23 }

// { dg-message "use '-fdiagnostics-all-candidates'" "" { target *-*-* } 21 }
// { dg-message "use '-fdiagnostics-all-candidates'" "" { target *-*-* } 22 }
// { dg-message "use '-fdiagnostics-all-candidates'" "" { target *-*-* } 23 }
