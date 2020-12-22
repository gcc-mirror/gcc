// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-alias" }

#include "merge-7.h"
import "merge-7_a.H";

// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(matched\) template_decl:'::template __promote_2<_Tp2,_Up2>::template __type'} module } }
// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
// { dg-final { scan-lang-dump-not {merge key \(unique\)} module } }
