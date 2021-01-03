// { dg-do run  }
// Test that a local declaration of one of a global overload set works

int f () { return 0; }
int f (int);

int main ()
{
  int f (); // { dg-warning "empty parentheses" }
  return f ();
}
