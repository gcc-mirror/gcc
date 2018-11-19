// { dg-module-do run }
// { dg-additional-options "-fmodule-legacy -fdump-lang-module" }
// { dg-module-bmi "\"macro-3_b.H\"" }

import "macro-3_a.H";

#ifndef MACRO_3b_H
#define MACRO_3b_H

#define bob 1
#undef foo
#undef bar
#define bar 3

#endif

// { dg-final { scan-lang-dump {Controlling macro is MACRO_3a_H} module } }
// { dg-final { scan-lang-dump {Read new macro #define foo at} module } }
// { dg-final { scan-lang-dump {Read new macro #define bar at} module } }
// { dg-final { scan-lang-dump-not {Read [^ ]* macro #define bob at} module } }

// { dg-final { scan-lang-dump {Selecting MACRO_3b_H as controlling} module } }
// { dg-final { scan-lang-dump {Writing macro #define bob at} module } }
// { dg-final { scan-lang-dump {Writing macro #undef & #define bar at} module } }
// { dg-final { scan-lang-dump {Writing macro #undef foo at} module } }
