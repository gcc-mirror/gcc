// Build don't link:

template <class T> struct S {};
template <class T = int> struct S<T*> {}; // ERROR - default argument

template <int I, int J> struct A {};
template <int I> struct A<I+5, I*2> {}; // ERROR - argument involves parameter

template <class T, T t> struct C {};
template <class T> struct C<T, 1>;  // ERROR - type depends on parameter
int i;
template <class T> struct C<T*, &i>; // ERROR - type depends on parameter

template< int X, int (*array_ptr)[X] > class B {};
int array[5];
template< int X > class B<X,&array> { }; // ERROR - type depends on parameter
