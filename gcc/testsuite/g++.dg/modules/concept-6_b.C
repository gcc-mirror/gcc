// { dg-additional-options "-fmodules-ts -fconcepts -fdump-lang-module-alias -fno-module-lazy" }

#include "concept-6.h"
import "concept-6_a.H";

// { dg-final { scan-lang-dump-times {named merge key \(matched\) function_decl:'::Derived<::._anon_0>::__ct '} 6 module } }
// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
