// Test that the named return value extension works when passed as a reference.
// Origin: Jason Merrill <jason@redhat.com>
// Special g++ Options: -Wno-deprecated

void f (int &i)
{
  i = 42;
}

int g () return r
{
  f (r);
}

int main ()
{
  int i = g ();
  return (i != 42);
}
