// PRMS Id: 4827
// Build don't link:

class A;
template <class T> int f (const T&, const A *);

int g (const int& a)
{
  return f (a, (A *)0); // gets bogus error - failed unification
}
