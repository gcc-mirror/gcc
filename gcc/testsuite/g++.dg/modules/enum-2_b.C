// { dg-additional-options -fmodules-ts }

import foo;

int i = X::frob;

int k = TPL<2>::v;

static_assert (!TPL<1>::u);
