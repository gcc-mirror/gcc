// PR c++/79050
// { dg-lto-do assemble }

int main ()
{
  extern auto foo ();
}
