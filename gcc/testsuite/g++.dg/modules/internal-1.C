// { dg-additional-options "-fmodules-ts" }

export module frob; // { dg-error "failed to write" }
// { dg-module-cmi !frob }

namespace {
// We shouldn't be complaining about members of internal linkage
// entities
class X  // { dg-bogus "internal linkage" "" { xfail *-*-* } }
{ // { dg-bogus "internal linkage" "" { xfail *-*-* } }
};

}

static int frob () 
{
  return 1;
}

export int f (int = frob ()); // { dg-error "references internal linkage" }
int goof (X &); // { dg-error "references internal linkage" }
