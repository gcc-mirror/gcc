// { dg-do run  }
// Test for handling of type shadowing in function scope.

int main()
{
  int A = 42;
  struct A
  {
    enum { a };
  };
  A = A::a;
  return A;
}
