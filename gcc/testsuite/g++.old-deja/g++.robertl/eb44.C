// spurious 'const' in error.
// For egcs-2.91.34, the warning message refers to
// class ostream & operator <<(class ostream &, const class Vector<T> &)
// Also, the template instantiation does not provide the missing
// friend function, the non-template function does

#include <stdio.h>
#include <iostream.h>

template <class T>
class Vector
{
  friend ostream& operator<< (ostream& out, const Vector<T> & vec); // WARNING - 
};

template <class T>
ostream& operator<< (ostream& out,  const Vector<T> & vec)
{
  abort();  // this should not be called
}

template class Vector<char>;
template ostream& operator<< (ostream& out,  const Vector<char> &);

ostream& operator<< (ostream& out, const Vector<char>&)
{
  return out;
}

int main()
{
  Vector<char> vc;
  ostream out;
  out << vc;
}
