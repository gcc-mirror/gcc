// { dg-additional-options "-fmodules-ts -fdump-lang-module-vops" }

import "macro-3_b.H";
import "macro-3_a.H";

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

// { dg-final { scan-lang-dump {Read new macro #define foo at} module } }
// { dg-final { scan-lang-dump {Read new macro #define bar at} module } }

// { dg-final { scan-lang-dump {Read add macro #undef foo} module } }
// { dg-final { scan-lang-dump {Read new macro #define bob} module } }
// { dg-final { scan-lang-dump {Read add macro #undef & #define bar} module } }
