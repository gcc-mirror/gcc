// { dg-do assemble  }
// GROUPS passed visibility
// Used to say:
// manip.cc:17: member `_f' is a private member of class `B<int>'
// manip.cc:17: member `_a' is a private member of class `B<int>'

class A {};

template <class TP>
class B;

template <class TP>
inline A &
operator<< (A &o, const B<TP> &m);

template <class TP>
class B
{
  A &(*_f) (A &, TP);
  TP _a;
public:
  B (A &(*f) (A &, TP), TP a) : _f (f), _a (a) {}
  friend A &operator<< <>(A &o, const B<TP> &m);
};

template <class TP>
inline A &
operator<< (A &o, const B<TP> &m)
{
  (*m._f) (o, m._a);
  return o;
}

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
