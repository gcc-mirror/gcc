// PR c++/80830

template <int> class a;
class b
{
  friend int operator>> (int, b);
};
template <int c> int &operator>> (int &, a<c> &);
template <int = 3> class a
{
  friend int &operator>><> (int &, a &);
  a<>
  d ()
  {
    return a<>();
  }
};
