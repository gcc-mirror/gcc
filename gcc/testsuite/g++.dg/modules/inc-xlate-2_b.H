// PR c++/110980
// { dg-additional-options "-fmodule-header -flang-info-include-translate" }
// { dg-module-cmi {} }

#include "inc-xlate-2_a.H"  // { dg-message "translated to import" }
