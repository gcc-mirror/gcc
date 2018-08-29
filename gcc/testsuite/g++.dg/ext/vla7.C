// PR c++/40013
// { dg-options "" }
// { dg-require-effective-target alloca }

template <class T>
struct A
{
  struct B
  {
    struct
    {
      int fn () { return 0; }
    } b;
  };
  void test ();
};

template <class T>
void
A <T>::test ()
{
  B a;
  int vla[a.b.fn ()];
}

int
main ()
{
  A <char> a;
  a.test ();
}
