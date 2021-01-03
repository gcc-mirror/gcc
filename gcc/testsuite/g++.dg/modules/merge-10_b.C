// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-alias" }

#include "merge-10.h"
import "merge-10_a.H";

// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
