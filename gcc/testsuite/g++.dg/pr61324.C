// { dg-do compile }
// { dg-options "-O -fkeep-inline-functions -fno-use-cxa-atexit" }
void foo ();

struct S
{
  ~S ()
  {
    foo ();
  }
};

S s;
