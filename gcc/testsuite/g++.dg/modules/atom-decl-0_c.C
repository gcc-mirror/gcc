// { dg-additional-options "-fmodules-ts" }
export module pop;
// { dg-module-cmi "pop" }
export import thing;

void bink ();

void bonk ()
{
  baz ();
  bink ();
}
