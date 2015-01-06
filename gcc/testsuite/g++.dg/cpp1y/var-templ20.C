// { dg-do compile { target c++14 } }

template <bool> struct Sink {};
template <class T> void fn();
template <class T> T var = T();

template <class T> void f()
{
  Sink<fn<T>::value>();		// { dg-error "function" }
  Sink<var<T>::value>();	// { dg-error "variable" }
}
// { dg-prune-output "template argument" }
