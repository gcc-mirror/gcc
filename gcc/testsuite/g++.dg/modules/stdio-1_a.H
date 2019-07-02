// { dg-module-do run }
// { dg-additional-options "-fmodule-header -fdump-lang-module" }
// { dg-module-cmi {} }

#ifndef _PROXY_STDIO_H
#define _PROXY_STDIO_H
/* Proxy stdio.h, because command line in flux.  */
#include <stdio.h>

#endif
// There should be *lots* of depsets (209 for glibc today)
// { dg-final { scan-lang-dump {Writing section:60 } module } }
// { dg-final { scan-lang-dump {Depset:0 decl function_decl:'::printf@[^\n]*stdio-1_a.H:1'} module } }
// { dg-final { scan-lang-dump {Depset:1 binding namespace_decl:'::printf'} module } }
// { dg-final { scan-lang-dump {Controlling macro=_PROXY_STDIO_H} module } }
