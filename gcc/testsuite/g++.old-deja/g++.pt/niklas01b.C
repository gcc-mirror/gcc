// Build don't link: 

template <class T> struct A { T *t; inline A() { t = 0; } };
template <class T> struct B : A<B<T> > { int x; inline B() { x = 3; } };

B<int> x;
