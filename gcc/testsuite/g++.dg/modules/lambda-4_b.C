// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-alias" }

#include "lambda-4.h"
import "lambda-4_a.H";

// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
// { dg-final { scan-lang-dump {named merge key \(matched\) template_decl:'::._anon_0::template _FUN'} module } }
// { dg-final { scan-lang-dump {named merge key \(matched\) template_decl:'::._anon_0::template __conv_op '} module } }
