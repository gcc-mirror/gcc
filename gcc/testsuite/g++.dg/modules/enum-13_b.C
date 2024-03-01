// PR c++/104919
// PR c++/106009
// { dg-additional-options -fmodules-ts }

import Enum13;

static_assert(f() == 42);
static_assert(g() == 43);
