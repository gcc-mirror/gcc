// { dg-additional-options "-fmodules-ts" }

export module frob; // { dg-error "failed to write" }
// { dg-module-bmi !frob }

namespace {
class X 
{
};

}

static int frob () 
{
  return 1;
}

export int f (int = frob ()); // { dg-error "references internal linkage" }
int goof (X &); // { dg-error "references internal linkage" }
