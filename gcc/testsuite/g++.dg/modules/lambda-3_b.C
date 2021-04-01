// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-alias" }

#include "lambda-3.h"
import "lambda-3_a.H";

// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
// { dg-final { scan-lang-dump {Read -[0-9]*\[0\] matched attached decl '::template ._anon_0<#unnamed#>'} module } }
// { dg-final { scan-lang-dump {Read -[0-9]*\[0\] matched attached decl '::._anon_2'} module } }
// { dg-final { scan-lang-dump {Read -[0-9]*\[0\] matched attached decl '::._anon_1'} module } }
