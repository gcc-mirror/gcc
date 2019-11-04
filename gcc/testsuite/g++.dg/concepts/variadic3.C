// PR c++/70036
// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts" }

template <class T> concept bool C = true;

template <class... T>
void f(T...) requires C<T>;	// { dg-error "parameter pack" }
