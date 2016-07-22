// PR c++/16029
// { dg-options "-Wunused" }

int main ()
{
  // We should not see an "unused" warning about "whatever" on the
  // next line.
  return whatever (); // { dg-error "10:'whatever' was not declared" }
}
