// { dg-additional-options "-fmodule-legacy -fmodule-mapper=|cxx-mapper\\ -f\\ $srcdir/g++.dg/modules/legacy-2.map" }
// { dg-module-bmi {"legacy-2_b.H"} }

#define frob FROB

// this should be diverted, if it isn't the above #define will break us
#include "legacy-2_a.H"
int move (int X = __LINE__); // Capture __LINE__ in a non-definition

// this should not be diverted
#include "legacy-2.h"


