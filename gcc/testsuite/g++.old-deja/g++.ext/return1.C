// { dg-do assemble  }
// { dg-options "-Wno-deprecated" }
// Test that the named return value extension works when passed as a reference.
// Origin: Jason Merrill <jason@redhat.com>

void f (int &i)
{
  i = 42;
}

int g () return r // { dg-error "" } named return value
{
  f (r); // { dg-error "" } undeclared
}

int main ()
{
  int i = g ();
  return (i != 42);
}
