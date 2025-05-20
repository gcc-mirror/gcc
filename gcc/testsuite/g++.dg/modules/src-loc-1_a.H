// PR c++/118904
// { dg-additional-options "-fmodule-header -std=c++20 -fdump-lang-module-uid" }
// { dg-module-cmi {} }

#include "src-loc-1.h"

// { dg-final { scan-lang-dump {Written:-[0-9]* internal identifier:[^\n\r]*Lsrc_loc} module } }
