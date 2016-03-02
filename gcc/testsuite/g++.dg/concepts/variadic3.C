// PR c++/70036
// { dg-do compile { target c++14 } }
// { dg-options "-fconcepts" }

template <class T> concept bool C = true;

template <class... T>
void f(T...) requires C<T>;	// { dg-error "parameter pack" }
