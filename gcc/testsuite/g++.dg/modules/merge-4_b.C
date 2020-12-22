// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-alias" }

#include "merge-4.h"
import "merge-4_a.H";

// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(matched\) template_decl:'::template Bob'} module } }
// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(matched\) template_decl:'::template Bob<T>::template M'} module } }
