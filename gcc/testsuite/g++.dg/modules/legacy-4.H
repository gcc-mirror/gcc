// { dg-do compile }
// { dg-additional-options "-fmodule-legacy=\"legacy-4.H\" -fmodule-mapper=|cxx-mapper\\ -f\\ $srcdir/g++.dg/modules/legacy-4.map" }

#include "nope"
int stop ();

// { dg-error "failed to read" "" { target *-*-* } 0 }
// { dg-prune-output "compilation terminated" }
// { dg-prune-output "fatal error:" }

// { dg-regexp "In module imported at \[^\n]*legacy-4.H:4:2:" }
