// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-alias" }

#include "vtt-2.h"
import "vtt-2_a.H";

// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
// { dg-final { scan-lang-dump-times {merge key \(unique\)} 2 module } }
