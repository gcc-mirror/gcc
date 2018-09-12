// { dg-additional-options "-std=c++2a -fmodules-atom" }

import "legacy-7_a.H";

#ifdef throw
#error barf
#endif

