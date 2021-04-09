// { dg-additional-options "-fmodules-ts" }

import tdef;

I main ()
{
  return 0;
}

J nope; // { dg-error "does not name a type" }

typedef char J;

static J ok;
