// { dg-options "-fmodules-atom" }
export module pop;
// { dg-module-bmi "pop" }
export import thing;

void bink ();

void bonk ()
{
  baz ();
  bink ();
}
