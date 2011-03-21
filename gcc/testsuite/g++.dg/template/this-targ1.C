// PR c++/47904

template <bool>
struct S
{
};

template <class T>
class U
{
  T t;
  int foo () const
  {
    S <sizeof (t) == 1> s;
    return 1;
  }
  int bar () const
  {
    S <sizeof (t) == 1> s;
    return 1;
  }
};

