// { dg-options "-Wall" }

template <class T>
int f (T t) { }			// { dg-warning "no return" }
