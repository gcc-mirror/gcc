// { dg-additional-options "-fmodules-ts" }

export module frob; // { dg-error "failed writing module 'frob' to 'frob.gcm':" }
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
