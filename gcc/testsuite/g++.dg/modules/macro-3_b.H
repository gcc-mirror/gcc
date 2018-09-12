// { dg-module-do run }
// { dg-additional-options "-fmodule-legacy=macro-3_b.H -Wno-pedantic -fdump-lang-module" }
// { dg-module-bmi "\"macro-3_b.H\"" }

import "macro-3_a.H";
;

#ifndef MACRO_3b_H
#define MACRO_3b_H

#define bob 1
#undef foo
#undef bar
#define bar 3

#endif

// { dg-final { scan-lang-dump {Reading #define MACRO_3a_H} module } }
// { dg-final { scan-lang-dump {Reading #define foo} module } }
// { dg-final { scan-lang-dump {Reading #define bar} module } }
// { dg-final { scan-lang-dump-not {Reading #define bob} module } }

// { dg-final { scan-lang-dump {Writing #undef foo} module } }
// { dg-final { scan-lang-dump {Writing #undef bar} module } }
// { dg-final { scan-lang-dump {Writing #define MACRO_3b_H} module } }
// { dg-final { scan-lang-dump {Writing #define bob} module } }
// { dg-final { scan-lang-dump {Writing #define bar} module } }
