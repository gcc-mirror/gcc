// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

#include "tpl-alias-1.h"

// { dg-final { scan-lang-dump {Writing named:-1 template_decl:'::allocator_traits<::allocator<long int>>::template rebind_alloc<_Up>'} module } }
// { dg-final { scan-lang-dump {Writing decl spec:-[0-9]* type_decl:'::allocator_traits<::allocator<long int>>::rebind_alloc<_Up>'} module } }
// { dg-final { scan-lang-dump {Writing:-[0-9]*'s decl spec merge key (specialization) type_decl:'::allocator_traits<::allocator<long int>>::rebind_alloc<_Up>'} module } }
// { dg-final { scan-lang-dump {Wrote(-[0-9]*) alias template type_decl:'::allocator_traits<::allocator<long int>>::rebind_alloc<_Up>'} module } }
