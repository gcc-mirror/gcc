// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-alias" }

#include "merge-9.h"
import "merge-9_a.H";

// { dg-final { scan-lang-dump {Read:-[10-9]*'s named merge key \(matched\) type_decl:'::std::align_val_t'} module } }
// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
// { dg-final { scan-lang-dump-not {merge key \(unique\)} module } }
