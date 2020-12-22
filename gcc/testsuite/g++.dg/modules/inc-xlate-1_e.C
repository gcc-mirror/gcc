// { dg-additional-options "-fmodules-ts -fmodule-mapper=|@g++-mapper-server\\ -t\\ [srcdir]/inc-xlate-1.map" }
export module bad;
#include "inc-xlate-1_a.H"  // { dg-error "not be include-translated" }

// { dg-prune-output "not writing module" }
