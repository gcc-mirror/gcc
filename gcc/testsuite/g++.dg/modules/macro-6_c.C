// { dg-additional-options "-fmodules-ts -fdump-lang-module" }
// Macro tables are only loaded when transitively reachable from top
// level

import macro;

#ifdef foo
#error bad
#endif

// { dg-final { scan-lang-dump-not {>Reading macro table "macro-6_a.H"} module } }
