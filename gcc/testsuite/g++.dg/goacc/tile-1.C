/*  of tile erroneously clobbered the template, resulting
    in missing errors and other fun.  */

template <int I>
void Foo ()
{
#pragma acc parallel loop tile(I) // { dg-error "" }
  for (int ix = 0; ix < 10; ix++)
    ;
}

int main ()
{
  Foo<1> ();  // OK
  Foo<-1> (); // error
}
