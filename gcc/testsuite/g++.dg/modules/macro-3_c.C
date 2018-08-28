// { dg-additional-options "-fmodules-atom -Wno-pedantic -fdump-lang-module" }

import "macro-3_b.H";
import "macro-3_a.H";

;

int main ()
{
#ifdef foo
  return 1;
#endif
  if (bar != 3)
    return 2;
#define foo 2
  if (foo != 2)
    return 3;
  return 0;
}

// { dg-final { scan-lang-dump {Reading #define MACRO_3a_H} module } }
// { dg-final { scan-lang-dump {Reading #define foo} module } }
// { dg-final { scan-lang-dump {Reading #define bar} module } }

// { dg-final { scan-lang-dump {Reading #undef foo} module } }
// { dg-final { scan-lang-dump {Reading #undef bar} module } }
// { dg-final { scan-lang-dump {Reading #define MACRO_3b_H} module } }
// { dg-final { scan-lang-dump {Reading #define bob} module } }
// { dg-final { scan-lang-dump {Reading #define bar} module } }


