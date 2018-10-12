// { dg-additional-options "-fmodules-atom -fmodule-mapper=|cxx-mapper\\ -f\\ $srcdir/g++.dg/modules/legacy-6.map" }

#include "legacy-6_a.H"
int i;
#include "legacy-6_b.H" // { dg-bogus "" "" { xfail *-*-* } }

