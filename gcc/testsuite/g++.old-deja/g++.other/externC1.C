// { dg-do assemble  }
// Origin: Mark Mitchell <mitchell@codesourcery.com>

extern "C"
{
  struct T
  {
    ~T ();
  };

  struct S
  {
    T t;
  };
}

S* s;

void f ()
{
  delete s;
}
