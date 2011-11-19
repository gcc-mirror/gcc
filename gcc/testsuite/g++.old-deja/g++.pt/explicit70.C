// { dg-do assemble  }

template <class T>
void f(T) {}

template <class T>
struct S {
  static T t;
};

template <class T>
T S<T>::t;

template void f(int);
template void f(int); // { dg-error "duplicate explicit instantiation" } 
template int S<int>::t;
template int S<int>::t; // { dg-error "duplicate explicit instantiation" } 
template class S<double>;
template class S<double>; // { dg-error "duplicate explicit instantiation" } 

extern template void f(double); // { dg-error "extern" "" { target c++98 } }
inline template class S<float>; // { dg-error "inline" } inline not allowed

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

template void g(int); // { dg-error "no definition available" "no def" }
// { dg-message "required" "inst" { target *-*-* } 43 }
