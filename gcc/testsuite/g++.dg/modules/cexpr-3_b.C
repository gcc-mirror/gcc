// PR c++/101449
// { dg-additional-options -fmodules-ts }

import pr101449;

static_assert(f().b);
static_assert(g(f()));
