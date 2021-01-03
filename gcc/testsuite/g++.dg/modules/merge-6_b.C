// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-alias" }

#include "merge-6.h"
import "merge-6_a.H";

// { dg-final { scan-lang-dump {Read:-[0-9]*'s field merge key \(matched\) template_decl:'::template __traitor<T>::template #null#'} module } }
// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
// { dg-final { scan-lang-dump-not {merge key \(unique\)} module } }
