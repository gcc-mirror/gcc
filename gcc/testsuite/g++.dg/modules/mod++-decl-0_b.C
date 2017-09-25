import thing;

void bink ()
{
  baz ();
  bar (); // { dg-error "" "" { xfail *-*-* } }
}
