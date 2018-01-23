// { dg-options "-fmodules++" }
module thing;

module
{
  void bink ();
}

void bonk ()
{
  baz ();
  bar (); // { dg-error "not declared" }
  bink ();
}
