// Contribued by Gabriel Dos Reis <gdr@codesourcery.com>
// Origin: iskey@i100.ryd.student.liu.se
// { dg-do link }

#include <iostream>
using namespace std;

template <class T> struct s;

template <class T>
ostream& operator<<(ostream &o, const typename s<T>::t &x)
{
  return o;
}

template <class T>
struct s {
  struct t
  {
    friend ostream&
    operator<<<T>(ostream&, const typename s<T>::t &);
  };
  t x;
};

int main()
{
  s<int>::t y;
  cout << y;
}
