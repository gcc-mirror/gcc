typedef void f ();
typedef f *pf;
long long i;

g ()
{
  long long p = i;
  ((pf) (long) p) ();
}
