// PR c++/56239
// { dg-do compile }

struct S
{
  int operator () () { return 0; }
};

int
main ()
{
  return (S ()) ();
}
