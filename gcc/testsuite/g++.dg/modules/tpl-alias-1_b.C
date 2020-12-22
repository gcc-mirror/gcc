// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-uid-alias" }

#include "tpl-alias-1.h"
import "tpl-alias-1_a.H";

// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(matched\) template_decl:'::allocator_traits<::allocator<long int>>::template rebind_alloc'} module } }
// { dg-final { scan-lang-dump {Read:-[0-9]*'s decl spec merge key \(matched\) type_decl:'::allocator_traits<::allocator<_Tp>>::rebind_alloc'} module } }
// { dg-final { scan-lang-dump {Read alias template type_decl:'::allocator_traits<::allocator<long int>>::rebind_alloc<_Up>'} module } }
// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
