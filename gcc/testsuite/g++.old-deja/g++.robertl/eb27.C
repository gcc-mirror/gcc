/* bug.cc */
/* simple program to demonstrate the bug with named return values in gcc
*/
/* (w) 4.9.97 by Kurt Garloff <K.Garloff@ping.de> */

#include <iostream.h>

// A simple numerical class
template <class T>
class test
{
   T elem;
 public:
   test ()  { elem = 0; };
   test (const T& a)  { elem = a; };
   test<T>& operator += (const test<T>& a)  { elem += a.elem; return *this; };
   friend test<T> operator + (const test<T>&, const test<T>&);
   friend ostream& operator << (ostream& os, const test<T>& a)
     { return os << a.elem; };
};

#ifndef NOBUG
// named return value version
template <class T>
test<T> operator + (const test<T>& a, const test<T>& b) return c(a);
{ c += b; };

#else
// equiv. version without named ret val
template <class T>
test<T> operator + (const test<T>& a, const test<T>& b)
{ test<T> c(a); c += b; return c; };

#endif

int main()
{
   test<int> x, y;
   x += 5; cout << x << endl;
   y = x + 2; cout << y << endl;
}

