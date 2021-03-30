// { dg-additional-options {-fmodules-ts -fno-module-lazy} }

import  "pr99283-5_b.H";

static_assert(!__traits<unsigned>::__min);
