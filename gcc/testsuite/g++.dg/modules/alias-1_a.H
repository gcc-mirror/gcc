// { dg-additional-options "-fmodule-legacy=alias-1_a.H -fmodule-mapper=|cxx-mapper\\ -f\\ $srcdir/g++.dg/modules/alias-1.map" }
// { dg-module-bmi {=alias-1.h} }

#ifndef ALIAS_1_A
#define ALIAS_1_A

int frob ();

#endif
