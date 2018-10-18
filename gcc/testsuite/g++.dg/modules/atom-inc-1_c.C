// { dg-additional-options "-fmodules-ts -fdump-lang-module" }

// These are different modules (by default).
import "frob";
import <frob>;

int i;

#ifndef QUOTE_FROB
#error QUOTE_FROB missing
#endif

#ifndef ANGLE_FROB
#error ANGLE_FROB missing
#endif

// { dg-final { scan-lang-dump {Starting module "frob"} module } }
// { dg-final { scan-lang-dump {Starting module <frob>} module } }
