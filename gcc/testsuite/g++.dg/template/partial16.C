// [temp.spec.partial.general]/9

template <class T, T t> struct C {};
template <class T> struct C<T, 1>;              // { dg-error "depends on a template parameter" }

template< int X, int (*array_ptr)[X] > class A {};
int array[5];
template< int X > class A<X,&array> { };        // { dg-error "depends on a template parameter" }
