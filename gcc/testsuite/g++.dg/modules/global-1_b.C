import thing;

void bink ()
{
  baz ();
  bar (); // { dg-error "not declared" ""  }
}
