// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-alias" }

#include "merge-8.h"
import "merge-8_a.H";

// { dg-final { scan-lang-dump {Deduping binfo '::__do_is_destructible_impl'\[0\]} module } }
// { dg-final { scan-lang-dump {Deduping binfo '::template __is_destructible_impl<_Tp>'\[0\]} module } }
// { dg-final { scan-lang-dump {Deduping binfo '::template __is_destructible_impl<_Tp>'\[1\]} module } }
