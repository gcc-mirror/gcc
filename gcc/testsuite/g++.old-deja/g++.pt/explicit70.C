// Build don't link:

template <class T>
void f(T) {}

template <class T>
struct S {
  static T t;
};

template <class T>
T S<T>::t;

template void f(int);
template void f(int); // ERROR - duplicate explicit instantiation 
template int S<int>::t;
template int S<int>::t; // ERROR - duplicate explicit instantiation 
template class S<double>;
template class S<double>; // ERROR - duplicate explicit instantiation 

extern template void f(double); // WARNING - extern not allowed
inline template class S<float>; // WARNING - inline not allowed

template <class T>
struct S<T*> {};

template class S<void*>; // OK - explicit instantiation of partial
                         // specialization

template <>
struct S<long double> {};

template class S<long double>; // OK - explicit instantiation after

template <>
void f(long double) {}

template void f(long double); // OK - explicit instantiation after

template <class T>
void g(T);

template void g(int); // ERROR - no definition of g.
