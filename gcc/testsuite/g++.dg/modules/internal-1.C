// { dg-additional-options "-fmodules-ts" }

export module frob;
// { dg-module-cmi !frob }

namespace
{
  // We shouldn't be complaining about members of internal linkage entities
  class X {};
}

static int frob () 
{
  return 1;
}

export int f (int = frob ()); // { dg-error "exposes TU-local entity" }
int goof (X &); // { dg-error "exposes TU-local entity" }
