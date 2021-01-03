// { dg-additional-options "-fmodule-header -fdump-lang-module-alias" }
// { dg-module-cmi {} }

#include "lambda-2.h"

// { dg-final { scan-lang-dump {Writing:-[0-9]*'s attached merge key \(decl\) type_decl:'::._anon_0'} module } }
// { dg-final { scan-lang-dump {Written -[0-9]*\[0\] attached decl '::._anon_0'} module } }
