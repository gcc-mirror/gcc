// { dg-do run  }
// Test that the integer hides the struct in block scope.

int main ()
{
  int A;
  struct A { };
  A = 1;
}
