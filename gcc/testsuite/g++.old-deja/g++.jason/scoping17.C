// Test that the integer hides the struct in block scope.

main ()
{
  int A;
  struct A { };
  A = 1;
}
