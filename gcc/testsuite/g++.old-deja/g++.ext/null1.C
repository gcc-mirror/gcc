// Test for overloading with g++ NULL.

void f (int *) { }
void f (char, char);
main ()
{
  f (__null);
}
