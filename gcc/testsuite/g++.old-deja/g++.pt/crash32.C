// Build don't link:
// Origin: Jason Merrill <jason@cygnus.com>

template <class T> struct A
{
  struct B;
};

template<class T> struct C
{
  friend typename A<T>::B;
};

