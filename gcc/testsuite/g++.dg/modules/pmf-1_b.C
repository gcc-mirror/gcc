// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-alias -fno-implicit-constexpr" }

#include "pmf-1.h"
import "pmf-1_a.H";

// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
// The function-scope var is unique
// { dg-final { scan-lang-dump-times {merge key \(unique\)} 1 module } }
