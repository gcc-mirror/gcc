// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-uid" }

#include "pmf-2.h"
import "pmf-2_a.H";

// { dg-final { scan-lang-dump {Cloned:-[0-9]* typedef template_type_parm:'::template remove_reference<_Tp>::template FOO<_Tp>'} module } }
// { dg-final { scan-lang-dump {Created:-[0-9]* ptrmem type} module } }
