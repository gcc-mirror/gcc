// { dg-do assemble  }

template <class T> struct S {};
template <class T = int> struct S<T*> {}; // { dg-error "" } default argument

template <int I, int J> struct A {};
template <int I> struct A<I+5, I*2> {}; // { dg-error "" } argument involves parameter

template <class T, T t> struct C {};
template <class T> struct C<T, 1>;  // { dg-error "" } type depends on parameter
int i;
template <class T> struct C<T*, &i>; // { dg-error "" } type depends on parameter

template< int X, int (*array_ptr)[X] > class B {};
int array[5];
template< int X > class B<X,&array> { }; // { dg-error "" } type depends on parameter
