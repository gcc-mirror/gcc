// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-alias" }

#include "merge-6.h"
import "merge-6_a.H";

// { dg-final { scan-lang-dump {Read:-16's named merge key \(matched\) template_decl:'::template __traitor<T>::template 0x0'} module } }
// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
// { dg-final { scan-lang-dump-not {merge key \(unique\)} module } }
