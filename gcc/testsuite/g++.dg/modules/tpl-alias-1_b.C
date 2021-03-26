// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-uid-alias" }

#include "tpl-alias-1.h"
import "tpl-alias-1_a.H";

// { dg-final { scan-lang-dump {Deduping '::allocator_traits<::allocator<_Tp>>::template rebind_alloc'} module } }
// { dg-final { scan-lang-dump {Deduping '::allocator_traits<::allocator<long int>>::template rebind_alloc<_Up>'} module } }
// { dg-final { scan-lang-dump {Deduping '::allocator_traits<::allocator<long int>>::rebind_alloc<long int>'} module } }

// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
