// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-alias" }

#include "merge-5.h"
import "merge-5_a.H";

// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(matched\) const_decl:'::template __traitor<T>::template X<T>::__value'} module } }
// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
// { dg-final { scan-lang-dump-not {merge key \(unique\)} module } }
