// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-alias" }

#include "merge-11.h"
import "merge-11_a.H";

// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
// { dg-final { scan-lang-dump-not {merge key \(unique\)} module } }
