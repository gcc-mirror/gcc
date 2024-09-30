// DR 2728 - Evaluation of conversions in a delete-expression
// { dg-do run }

struct S {
  S (int *x) : p (x) {}
  operator int * () const { ++s; return p; }
  int *p;
  static int s;
};
int S::s;

int
main ()
{
  int *a = new int;
  S s (a);
  delete s;
  if (S::s != 1)
    __builtin_abort ();
}
