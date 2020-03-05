// { dg-additional-options "-fmodules-ts -Wno-pedantic -fno-module-lazy -fdump-lang-module-alias" }

#include "tpl-ary-1.h"
import "tpl-ary-1_a.H";

// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
// { dg-final { scan-lang-dump-not {merge key \(unique\)} module } }
