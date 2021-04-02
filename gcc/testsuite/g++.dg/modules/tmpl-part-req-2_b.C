// { dg-additional-options "-std=c++2a -fmodules-ts -fno-module-lazy -fdump-lang-module-alias" }

#include "tmpl-part-req-2.h"
import "tmpl-part-req-2_a.H";

// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
