// PR c++/61614
// { dg-options "" }

int Fn (...);

void
Test ()
{
  int j = Fn ((const int[]) { 0 });                    // OK
  unsigned long sz = sizeof Fn ((const int[]) { 0 });  // Error
}
