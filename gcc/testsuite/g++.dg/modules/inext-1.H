// { dg-additional-options "-fmodule-header -isystem [srcdir] -isystem [srcdir]/sys -fdump-lang-module" }

#ifndef _PROTECT
#define _PROTECT

/* We were found on the system inc path, so have been turned into a
   system header, so no warning on the following extension.  */
#include_next <inext-1.H>

#endif

// { dg-final { scan-lang-dump {Wrote section:1 named-by:'::foo'} module } }
