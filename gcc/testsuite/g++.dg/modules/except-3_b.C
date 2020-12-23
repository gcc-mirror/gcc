// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-alias" }

#include "except-3.h"
import "except-3_a.H";

// { dg-final { scan-lang-dump-times {merge key \(new\) function_decl:'::_Tuple_impl<int>::__[cd]t '} 3 module } }
// { dg-final { scan-lang-dump-times {Propagating instantiated noexcept to '::_Tuple_impl<int>::__ct <int>'} 1 module } }
