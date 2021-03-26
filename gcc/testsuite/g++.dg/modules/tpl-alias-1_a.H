// { dg-additional-options "-fmodule-header -fdump-lang-module-alias-uid" }
// { dg-module-cmi {} }

#include "tpl-alias-1.h"

// { dg-final { scan-lang-dump {Writing decl tmpl spec:-[0-9]* template_decl:'::allocator_traits<::allocator<long int>>::template rebind_alloc<_Up>'} module } }
// { dg-final { scan-lang-dump {Writing decl tmpl spec:-[0-9]* type_decl:'::allocator_traits<::allocator<long int>>::template rebind_alloc<_Up>'} module } }

// { dg-final { scan-lang-dump {Writing:-[0-9]*'s alias spec merge key \(specialization\) type_decl:'::allocator_traits<::allocator<long int>>::rebind_alloc<long int>'} module } }
