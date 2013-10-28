// Test for printing the type of T{} in error messages.
// { dg-options -std=c++11 }

template <class T, T t> struct A { };
template <class T> A<T,T{}> f(T t); // { dg-message "T{}" }

int main()
{
  f();				// { dg-error "no match" }
}
