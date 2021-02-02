// { dg-module-do link }
// { dg-additional-options "-fmodules-ts" }
export module frob;
// { dg-module-cmi frob }

namespace details
{
void foo ()
{
}
}

using details::foo;
void footle ();
export void bink ()
{
  foo ();
  footle ();
}
