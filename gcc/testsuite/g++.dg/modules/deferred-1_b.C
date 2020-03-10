// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-alias" }

#include "deferred-1.h"
import "deferred-1_a.H";

// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
// { dg-final { scan-lang-dump-not {merge key \(unique\)} module } }
