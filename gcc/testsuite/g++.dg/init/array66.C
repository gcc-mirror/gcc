// PR c++/117827
// { dg-do run { target c++11 } }

struct C {
  int c;
  static int d, e;
  C () : c (0) { ++d; }
  C (const C &) = delete;
  C &operator= (const C &) = delete;
  ~C () { ++e; }
};
int C::d, C::e;

C *
foo (C *p)
{
  delete[] p;
  throw 1;
}

int
main ()
{
  try
    {
      foo (new C[1] {});
    }
  catch (...)
    {
    }
  if (C::d != C::e)
    __builtin_abort ();
}
