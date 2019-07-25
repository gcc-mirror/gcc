// { dg-module-do run }
// { dg-additional-options "-fmodule-header -fdump-lang-module-vops" }
// { dg-module-cmi {} }

#ifndef MACRO_3a_H
#define MACRO_3a_H

#undef nothing
#define bob x
#undef bob
#define foo 1
#define bar 2

#endif

// { dg-final { scan-lang-dump {Writing macro #define foo at} module } }
// { dg-final { scan-lang-dump {Writing macro #define bar at} module } }
// { dg-final { scan-lang-dump-not {Writing macro #define bob at} module } }
// { dg-final { scan-lang-dump-not {Writing macro #undef nothing at} module } }
