// { dg-additional-options "-fmodules-ts -fmodule-mapper=|cxx-mapper\\ -f\\ [srcdir]/inc-xlate-1.map" }
export module bad;
#include "inc-xlate-1_a.H"  // { dg-warning "fragile" }
