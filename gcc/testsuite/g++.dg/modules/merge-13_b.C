// { dg-additional-options "-fmodules-ts -fconcepts -fno-module-lazy -fdump-lang-module-alias" }

#include "merge-13.h"
import "merge-13_a.H";

// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
// { dg-final { scan-lang-dump-not {merge key \(unique\)} module } }
