// { dg-additional-options "-fmodules-ts -std=c++2a" }

import "legacy-7_a.H";

#ifdef throw
#error barf
#endif

