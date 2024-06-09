// { dg-require-effective-target c++20 }
// { dg-additional-options "-fmodules-ts -fconcepts -fdump-lang-module-alias -fno-module-lazy" }

#include "concept-8.h"
import "concept-8_a.H";

// { dg-final { scan-lang-dump-times {named merge key \(matched\) function_decl:'::Base<::._anon_0>::__ct '} 2 module } }
// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
