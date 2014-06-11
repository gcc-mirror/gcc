// { dg-do compile }
// { dg-options "-O2 -std=c++11 -Werror=uninitialized" }

int rand ();

class Funcs
{
public:
    int *f1 ();
    int *f2 ();
};
typedef decltype (&Funcs::f1) pfunc;

static int Set (Funcs * f, const pfunc & fp)
{
  (f->*fp) ();
}

void
Foo ()
{
  pfunc fp = &Funcs::f1;
  if (rand ())
    fp = &Funcs::f2;
  Set (0, fp);
}
