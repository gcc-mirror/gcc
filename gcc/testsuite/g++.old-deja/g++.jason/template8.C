// { dg-do assemble  }
// PRMS Id: 4827

class A;
template <class T> int f (const T&, const A *);

int g (const int& a)
{
  return f (a, (A *)0); // { dg-bogus "" } failed unification
}
