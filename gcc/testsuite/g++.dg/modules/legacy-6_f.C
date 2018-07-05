// { dg-additional-options "-fmodules-atom -fmodule-mapper=|cxx-mapper\\ -f\\ $srcdir/g++.dg/modules/legacy-6.map" }

#include "legacy-6_a.H"
int i; // { dg-message "ended here" }
#include "legacy-6_b.H" // { dg-warning "cannot be a legacy module" }
