// Build don't link: 

template <class X> struct A { int operator [] (int); };
template <class Y> int A<Y>::operator[] (int j) { return j * j; }

extern A<void **> avpp;

int q () { return avpp[99]; }
