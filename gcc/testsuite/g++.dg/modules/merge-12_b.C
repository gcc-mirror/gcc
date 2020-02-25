// { dg-additional-options "-fmodules-ts -fconcepts -fno-module-lazy -fdump-lang-module-alias" }

#include "merge-12.h"
import "merge-12_a.H";

// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
// { dg-final { scan-lang-dump-not {merge key \(unique\)} module } }
