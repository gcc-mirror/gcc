// spurious 'const' in error.

#include <stdio.h>
#include <iostream.h>

template <class T>
class Vector
{
  friend ostream& operator<< (ostream& out, const Vector<T> & vec);
};

template <class T>
ostream& operator<< (ostream& out,  const Vector<T> & vec)
{}

template class Vector<char>;
template ostream& operator<< (ostream& out,  const Vector<char> &);

main()
{
  Vector<char> vc;
  ostream out;
  out << vc;
}
