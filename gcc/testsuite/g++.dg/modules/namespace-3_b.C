// { dg-additional-options "-fmodules-ts" }

import frob;

int x = silent; // { dg-error "not declared" }

static int silent;

int user ()
{
  return f (silent);
}
