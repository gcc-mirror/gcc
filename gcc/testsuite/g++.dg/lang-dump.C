// { dg-additional-options "-fdump-lang-all" }
// Just check we don't explode when asking for language dumps.  Does
// not necessarily mean any particular language dump is useful.

struct X 
{
  int m;
  virtual ~X ();
};

X::~X () {}

struct Y : X
{
};

int frob (int a)
{
  return 2 * a;
}

