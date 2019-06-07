// { dg-additional-options "-fmodules-ts -fforce-module-macros" }

import "macro-5_a.H";
import "macro-5_b.H";

#if baz != 3
#error
#endif
