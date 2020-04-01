// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-alias" }
// Not an ODR violation!
#include "lambda-2.h"
import "lambda-2_a.H";

// { dg-bogus "conflicting" "not an odr violation" }
// { dg-final { scan-lang-dump {Read:-[0-9]*'s attached merge key \(matched\) type_decl:'#null#'} module } }
// { dg-final { scan-lang-dump {Read -[0-9]*\[0\] matched attached decl '::._anon_0'} module } }
