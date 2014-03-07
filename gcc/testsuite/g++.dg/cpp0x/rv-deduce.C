// PR c++/36816, core issue 873
// { dg-do compile { target c++11 } }

template <class T> void h (T&&) { }

void (*pf)(int&)   = &h;
template <> void h(char&);
template void h(double&);
