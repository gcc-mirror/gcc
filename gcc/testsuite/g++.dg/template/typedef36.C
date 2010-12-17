// Origin: PR c++/45606
// { dg-do compile }

template<class T, class U = int>
struct S0
{
  typedef int const_iterator;
};

template<class T>
struct Test
{
  typedef S0<T> SAlias;
  typedef typename SAlias::const_iterator const_iterator;
  const_iterator begin ();
};

template<class T>
typename S0<T>::const_iterator
Test<T>::begin()
{
  return 0;
}
