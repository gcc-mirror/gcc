// { dg-do assemble  }
// GROUPS passed conversions
class A {};

template <class TP>
class B
{
  A &(*_f) (A &, TP);
  TP _a;
public:
  B (A &(*f) (A &, TP), TP a) : _f (f), _a (a) {}
  friend A &operator<< (A &o, const B<TP> &m)
    { (*m._f) (o, m._a); return o; }
};

A &setw (A &, int);
B<int> setw (int n)
{
  return B<int> (setw, n);
}

A x;

void f ()
{
  x << setw (2);
}
