/* bug.cc */
/* simple program to demonstrate the bug with named return values in gcc
*/
/* (w) 4.9.97 by Kurt Garloff <K.Garloff@ping.de> */
// Special g++ Options:
// 8/28/1998 - This dies in add_conversions from dfs_walk, null CLASSTYPE_METHOD_VEC
// for the test<T> record_type.  This is marked as an expected failure for now,
// until we actually fix it.

#include <iostream.h>

template <class T> class test;
template <class T> test<T> operator + (const test<T>& a, const test<T>& b);

// A simple numerical class
template <class T>
class test
{
   T elem;
 public:
   test ()  { elem = 0; };
   test (const T& a)  { elem = a; };
   test<T>& operator += (const test<T>& a)  { elem += a.elem; return *this; };
   friend test<T> operator + <> (const test<T>&, const test<T>&);
   friend ostream& operator << (ostream& os, const test<T>& a)
     { return os << a.elem; };
};

// named return value version
template <class T>
test<T> operator + (const test<T>& a, const test<T>& b) return c(a);
{ c += b; };

int main()
{
   test<int> x, y;
   x += 5; cout << x << endl;
   y = x + test<int>(2); cout << y << endl;
}
