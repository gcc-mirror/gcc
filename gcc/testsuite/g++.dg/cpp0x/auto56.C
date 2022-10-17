// Check that we don't prefer #2 because it's more specialized.
// { dg-do compile { target c++11 } }

template <class T> T f(T);
template <class T> T* f(T*);
auto p = &f<int>;		// { dg-error "" }
