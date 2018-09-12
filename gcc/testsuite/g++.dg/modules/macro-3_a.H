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

// { dg-final { scan-lang-dump {Writing #define MACRO_3a_H} module } }
// { dg-final { scan-lang-dump {Writing #define foo} module } }
// { dg-final { scan-lang-dump {Writing #define bar} module } }
// { dg-final { scan-lang-dump-not {Writing #define bob} module } }
