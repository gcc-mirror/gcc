// { dg-additional-options "-fmodules-ts" }
import thing;

void bink ()
{
  baz ();
  bar (); // { dg-error "not declared" ""  }
}
