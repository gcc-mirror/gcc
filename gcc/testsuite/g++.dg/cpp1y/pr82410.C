// PR c++/82410
// { dg-do compile { target c++14 } }

int
main ()
{
  struct A {};
  struct S
  {
    int & p;
    int x = p;
    operator A () { return {}; }
  };
  int l;
  [] (A) {} (S{l});
}
