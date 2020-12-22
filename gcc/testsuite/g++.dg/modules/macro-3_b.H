// { dg-module-do run }
// { dg-additional-options "-fmodule-header -fdump-lang-module-vops" }
// { dg-module-cmi {} }

import "macro-3_a.H";

// Not the controlling macro, because of above tokens
#ifndef MACRO_3b_H
#define MACRO_3b_H

#define bob 1
#undef foo
#undef bar
#define bar 3

#endif

// { dg-final { scan-lang-dump {Read new macro #define foo at} module } }
// { dg-final { scan-lang-dump {Read new macro #define bar at} module } }
// { dg-final { scan-lang-dump-not {Read [^ ]* macro #define bob at} module } }

// { dg-final { scan-lang-dump {Writing macro #define bob at} module } }
// { dg-final { scan-lang-dump {Writing macro #undef & #define bar at} module } }
// { dg-final { scan-lang-dump {Writing macro #undef foo at} module } }
