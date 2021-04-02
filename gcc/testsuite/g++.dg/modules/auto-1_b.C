// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-alias" }

#include "auto-1.h"
import "auto-1_a.H";

int bar ()
{
  return foo () + frob (0u);
}

// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
