// Build don't link: 
// Special g++ Options:
template<class X> struct A {
  A ();
  ~A();
  int x, y, z;
};

template <class Y> inline A<Y>::A () { x = y = 3; z = 99; }
template <class Z> inline A<Z>::~A() { y = 9999; }

A<int> ai;
