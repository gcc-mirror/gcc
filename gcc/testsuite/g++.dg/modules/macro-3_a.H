// { dg-module-do run }
// { dg-additional-options "-fmodule-legacy=macro-3_a.H -fdump-lang-module" }
// { dg-module-bmi "\"macro-3_a.H\"" }

#ifndef MACRO_3a_H
#define MACRO_3a_H

#undef nothing
#define bob x
#undef bob
#define foo 1
#define bar 2

#endif

// { dg-final { scan-lang-dump {Selecting MACRO_3a_H as controlling} module } }
// { dg-final { scan-lang-dump {Writing macro #define foo at} module } }
// { dg-final { scan-lang-dump {Writing macro #define bar at} module } }
// { dg-final { scan-lang-dump-not {Writing macro #define bob at} module } }
// { dg-final { scan-lang-dump-not {Writing macro #undef nothing at} module } }
