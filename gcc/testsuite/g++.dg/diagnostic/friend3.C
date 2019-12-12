// { dg-do compile }

void
fn ()
{
  struct S {
    friend void bar () { } // { dg-error "17:cannot define friend function 'bar' in a local class definition" }
  };
}
