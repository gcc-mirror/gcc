// { dg-additional-options -fmodules-ts }
export module bad;
// { dg-module-cmi !bad }

namespace N
{
static int foo ();
int bar ();
}

using N::foo;
using N::bar;

export using N::foo; // { dg-error "does not have external linkage" }
export using N::bar; // { dg-error "does not have external linkage" }
