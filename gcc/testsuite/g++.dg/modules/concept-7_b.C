// PR c++/102963
// { dg-additional-options "-fmodules-ts -fconcepts" }

import pr102963;

static_assert(C<int>);
static_assert(C<void>); // { dg-error "static assert" }
