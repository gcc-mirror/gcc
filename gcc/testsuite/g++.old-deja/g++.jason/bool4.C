// Test for allowing conversion to bool.

struct A { };

main ()
{
  bool b = (void*)0;
  b = (int A::*)0;
  b = (int (A::*)())0;
}
