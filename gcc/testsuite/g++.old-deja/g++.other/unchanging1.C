// { dg-do run  }
// { dg-options "-O2" }

#include <iostream>
#include <complex>

using namespace std;

class A {
protected:
  int a;
  complex<double> *b;
public:
  A(int n);
  inline complex<double>& operator[] (int x);
};

A::A(int n)
{
  a = n;
  b = new complex<double>[a];
  for (int i=0; i<a; i++) b[i] = complex<double>(0.0,0.0);
}

inline complex<double>& A::operator[](int x)
{
  if (x < 0 || x >= a)
    cout << "x error" << endl;
  return b[x];
}

void foo ()
{
  int n = 5;
  A *o = new A(n);
  A *p = new A(n);
  for (int i = 0; i < n; i++) {
    cout << i << endl;
    (*o)[i] *= complex<double>((*p)[i].real(), (*p)[i].imag());
  }
}

int main()
{
  foo();
}
