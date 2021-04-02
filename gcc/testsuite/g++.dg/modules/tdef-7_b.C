// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-alias-uid" }

#include "tdef-7.h"
import "tdef-7_a.H";

// { dg-final { scan-lang-dump-times {merge key \(matched\) function_decl:'::duration_cast} 1 module } }
// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
// { dg-final { scan-lang-dump-times {merge key \(unique\) type_decl:'#null#'} 2 module } }
// { dg-final { scan-lang-dump-times {Cloned:-[0-9]* typedef integer_type:'::duration_cast::__to_rep'} 1 module } }
