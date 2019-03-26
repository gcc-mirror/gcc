// { dg-module-do run }
// { dg-additional-options "-fmodule-header -fdump-lang-module" }
// { dg-module-bmi {} }

/* Proxy stdio.h, because command line in flux.  */
#include <stdio.h>

// There should be *lots* of depsets (209 for glibc today)
// { dg-final { scan-lang-dump {Writing SCC:60 } module } }
// { dg-final { scan-lang-dump {Depset:0 declaration function_decl:'::printf'} module } }
// { dg-final { scan-lang-dump {Depset:1 binding namespace_decl:'::printf'} module } }
// { dg-final { scan-lang-dump {Controlling macro=_STDIO_H} module } }
