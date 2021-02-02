// { dg-additional-options "-fmodules-ts -fconcepts -fdump-lang-module-alias -fno-module-lazy" }

#include "auto-2.h"
import "auto-2_a.H";

// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
